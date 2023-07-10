package main

import (
	"testing"
)

func mapEqual(a, b map[string]string) bool {
	if len(a) != len(b) {
		return false
	}

	for k, v := range a {
		bv, ok := b[k]
		if !ok {
			return false
		}

		if v != bv {
			return false
		}
	}

	return true
}

func sliceEqual(a, b []byte) bool {
	if len(a) != len(b) {
		return false
	}

	for i, item := range a {
		if item != b[i] {
			return false
		}
	}

	return true
}

func responseEqual(a, b Response) bool {
	return mapEqual(a.Header, b.Header) && a.Status == b.Status && sliceEqual(a.Body, b.Body) && sliceEqual(a.Database, b.Database)
}

func TestHomePage(t *testing.T) {
	input := Request{
		Header:        map[string][]string{},
		Method:        "GET",
		Path:          "",
		Form:          map[string][]string{},
		Database:      []byte(""),
		DatabaseError: nil,
		ParseError:    nil,
	}
	expected := Response{
		Header:   map[string]string{},
		Status:   200,
		Body:     []byte(indexHtml),
		Database: []byte(""),
	}
	got := server(input)
	if !responseEqual(expected, got) {
		t.Errorf("expecting %v but got %v", expected, got)
	}
}
