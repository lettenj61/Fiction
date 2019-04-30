port module Main exposing (..)

import Browser
import Html exposing (..)



-- MAIN


main : Program ( Maybe (List String) ) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { samples : List String
    }


init : ( Maybe (List String) ) -> ( Model, Cmd Msg )
init maybeSamples =
    ( { samples = Maybe.withDefault [] maybeSamples }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view _ =
    text "not implemented"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
