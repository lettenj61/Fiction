module Fiction.Translate exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Random as Random exposing (Generator)
import Fiction.Token as Token exposing (Analyzer, Token)



type Translator
    = Translator


type alias TranslatorConfig =
    { 
    }


type alias Mapping =
    Dict String String


-- TRANSLATIONS



-- HELPERS


newDict : Analyzer -> TranslatorConfig -> ( Generator Mapping )
newDict { tokens } config =
    let
        normalTones =
            List.filter
                (\tok -> not <| Token.isSingleVowel_ tok)
                ( Dict.keys tokens )

        consumer =
            case normalTones of
                hd :: tl ->
                    Random.uniform hd tl
                _ ->
                    Random.uniform "" []
    in
    Random.constant Dict.empty -- TODO
