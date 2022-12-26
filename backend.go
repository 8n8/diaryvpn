package main

import (
	_ "embed"
	"encoding/json"
	"fmt"
	"net/http"
	"os"
)

func main() {
	http.HandleFunc("/", rootHandler)
    http.HandleFunc("/elm.js", elmJsHandler)
	http.HandleFunc("/summary", summaryHandler)
	http.HandleFunc("/favicon.ico", faviconHandler)
	http.ListenAndServe(":8080", nil)
}

//go:embed index.html
var indexHtml []byte

func rootHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Add("Content-Type", "text/html")
	w.Write(indexHtml)
}

//go:embed elm.js
var elmJs []byte

func elmJsHandler(w http.ResponseWriter, r *http.Request) {
    w.Header().Add("Content-Type", "text/javascript")
    w.Write(elmJs)
}

//go:embed favicon.ico
var faviconIco []byte

func faviconHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Add("Content-Type", "image/x-icon")
	w.Write(faviconIco)
}

func summaryHandler(w http.ResponseWriter, r *http.Request) {
	raw, err := os.ReadFile("db")
	entries := []Entry{}
	if err != nil && !os.IsNotExist(err) {
		panic(fmt.Errorf("could not read database file: %s\n", err))
	}

	if !os.IsNotExist(err) {
		err = json.Unmarshal(raw, &entries)
		if err != nil {
			panic(err)
		}
	}

	encoded, err := json.Marshal(getSummaries(entries))
	if err != nil {
		panic(err)
	}

	w.Header().Add("Content-Type", "application/json")
	w.Write(encoded)
}

func makeFragment(entry string) string {
	charOffset := 0
	for byteOffset := range entry {
		if charOffset == 50 {
			return entry[:byteOffset]
		}
		charOffset++
	}
	return entry
}

func getSummaries(entries []Entry) []Summary {
	summaries := make([]Summary, len(entries), len(entries))
	for i := 0; i < len(summaries); i++ {
		summary := Summary{
			Timestamp: entries[i].Timestamp,
			Fragment:  makeFragment(entries[i].Text),
		}
		summaries[i] = summary
	}
	return summaries
}

type Summary struct {
	Timestamp uint32 `json:"timestamp"`
	Fragment  string `json:"fragment"`
}

type Entry struct {
	Timestamp uint32 `json:"timestamp"`
	Text      string `json:"text"`
}
