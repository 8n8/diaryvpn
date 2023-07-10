package main

import (
    _ "embed"
)

type Request struct {
    Header map[string]string
    Method string
    Path string
    Form map[string][]string
    Database []byte
    DatabaseError error
}

type Response struct {
    Header map[string]string
    Status int
    Body []byte
    Database []byte
}

//go:embed dist/index.html
var indexHtml []byte


func server(request Request) Response {
    return Response{
        Header: map[string]string{},
        Status: 200,
        Body: indexHtml,
        Database: []byte(""),
    }
}
