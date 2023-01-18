module App exposing (Model(..), Msg(..), defaultFlags, init, update, view)

import Browser
import ProgramTest exposing (SimulatedEffect)
import SimulatedEffect.Cmd
import Url exposing (Url)


type Msg
    = Msg
    | OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url


init : () -> Url -> () -> ( Model, SimulatedEffect Msg )
init () _ () =
    ( Model, SimulatedEffect.Cmd.none )


type Model
    = Model


update : Msg -> Model -> ( Model, SimulatedEffect Msg )
update _ _ =
    ( Model, SimulatedEffect.Cmd.none )


defaultFlags : ()
defaultFlags =
    ()


view : Model -> Browser.Document Msg
view _ =
    { title = "DiaryVPN"
    , body = []
    }
