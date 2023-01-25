module Entry exposing (Entry, decode)

import Json.Decode as Decode exposing (Decoder)
import Time

type Entry
    = Entry
        { timestamp : Time.Posix
        , entry : String
        }


decode : Decoder Entry
decode =
    Decode.map2 (\t e -> Entry { timestamp = t, entry = e})
        (Decode.field "timestamp"
            (Decode.map Time.millisToPosix Decode.int))
        (Decode.field "entry" Decode.string)

        
