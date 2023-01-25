module Db exposing (Db, decode)


import Array exposing (Array)
import Entry exposing (Entry)
import Json.Decode as Decode exposing (Decoder)

type Db
    = Db (Array Entry)
        

decode : Decoder Db
decode =
    Decode.map Db (Decode.array Entry.decode)
