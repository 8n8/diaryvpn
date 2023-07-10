package main

import (
    "testing"
)

func TestHomePage(t *testing.T) {
    input := Request{
        Header: map[string]string{},
        Method: "GET",
        Path: "",
        Form: map[string][]string{},
        Database: []byte(""),
    }
    expected := Response{
        Header: map[string]string{},
        Status: 200,
        Body: []byte(homePageHtml),
        Database: []byte(""),
    }
    got := server(request)
    if expected != got {
        t.Errorf("expecting %v but got %v", expected, got)
    }
}
