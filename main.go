package main

import (
	_ "embed"
	"net/http"
	"os"
	"sync"
)

type Request struct {
	Header        map[string][]string
	Method        string
	Path          string
	Form          map[string][]string
	Database      []byte
	DatabaseError error
	ParseError    error
}

type Response struct {
	Header   map[string]string
	Status   int
	Body     []byte
	Database []byte
}

//go:embed dist/index.html
var indexHtml []byte

func server(request Request) Response {
	return Response{
		Header:   map[string]string{},
		Status:   200,
		Body:     indexHtml,
		Database: []byte(""),
	}
}

func main() {
	var lock sync.Mutex

	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		parseError := r.ParseForm()
		request := Request{
			Header:        map[string][]string(r.Header),
			Method:        r.Method,
			Path:          r.URL.Path,
			ParseError:    parseError,
			Form:          map[string][]string(r.Form),
			Database:      []byte{},
			DatabaseError: nil,
		}

		lock.Lock()
		database, databaseError := os.ReadFile("database.json")
		request.Database = database
		request.DatabaseError = databaseError
		response := server(request)
		_ = os.WriteFile("database.json", response.Database, 0600)
		lock.Unlock()

		for headerKey, headerValue := range response.Header {
			w.Header().Set(headerKey, headerValue)
		}
		w.WriteHeader(response.Status)
		_, _ = w.Write(response.Body)
	})

	_ = http.ListenAndServe(":8080", nil)
}
