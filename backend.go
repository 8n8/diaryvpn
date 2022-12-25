package main


import (
    "net/http"
    _ "embed"
)


type Db struct {
    lock sync.Mutex
    diary Diary
}


// Say you write an entry a day for 70 years, that is 365 * 70 ~= 25000
// entries. Say 30,000.
const maxEntries = 30000


// I read in the Guardian that Tony Benn's diaries contain about 20m
// words, and he wrote it for all of his life. Say he wrote 30,000 entries,
// and that there were 6 characters per word on average, that is
// 20,000,000 * 5 / 30,000 ~= 3333 characters per entry on average.
const entrySize = 3333


type Diary struct {
    num int
    ends [maxEntries]uint32
    texts [maxEntries * entrySize]
}


func main() {
    http.HandleFunc("/", serveIndexHtml)
    http.HandleFunc("/summaries", serveSummaries)

    http.ListenAndServe(":8080", nil)
}

//go:embed index.html
var indexHtml []byte

func serveIndexHtml(w http.ResponseWriter, r *http.Request) {
    w.Header().Set("Content-Type", "text/html")
    w.Write(indexHtml)
}

func serveSummaries(w http.ResponseWriter, r *http.Request) {
}
