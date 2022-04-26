port module Common exposing (Flags, Model, Msg, program)

import Elm exposing (File)


port output : String -> Cmd msg


type alias Flags =
    String


type alias Model =
    ()


type alias Msg =
    ()


program : (Flags -> File) -> Program Flags Model Msg
program flagsToFile =
    Platform.worker
        { init = init flagsToFile
        , update = update
        , subscriptions = subscriptions
        }


init : (Flags -> File) -> Flags -> ( Model, Cmd Msg )
init flagsToFile flags =
    ( (), output (flagsToFile flags).contents )


update : Msg -> Model -> ( Model, Cmd Msg )
update _ _ =
    ( (), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
