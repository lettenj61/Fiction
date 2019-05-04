module Fiction.Translate exposing (shuffled)

import Array exposing (Array)
import Dict exposing (Dict)
import Fiction.Token as Token exposing (Analyzer, Token)
import Random as Random exposing (Generator)


type Translator
    = Translator


type alias TranslatorConfig =
    {}


type alias Mapping =
    Dict String String



-- TRANSLATIONS
-- HELPERS


singleVowelMapping : Generator Mapping
singleVowelMapping =
    let
        chars =
            [ "a", "e", "i", "o", "u" ]

        makePairs maybes =
            Array.toList maybes
                |> List.filterMap identity
                |> List.map2 Tuple.pair chars
    in
    Random.map
        (makePairs >> Dict.fromList)
        (shuffled <| Array.fromList chars)


newDict : Analyzer -> TranslatorConfig -> Generator Mapping
newDict { tokens } config =
    let
        normalTones =
            List.filter
                (\tok -> not <| Token.isSingleVowel_ tok)
                (Dict.keys tokens)

        consumer =
            case normalTones of
                hd :: tl ->
                    Random.uniform hd tl

                _ ->
                    Random.uniform "" []
    in
    Random.constant Dict.empty


{-| Returns input array except element at nth index.
-}
exceptN : Int -> Array a -> Array a
exceptN n array =
    if n < 0 then
        array

    else
        Array.append
            (Array.slice 0 n array)
            (Array.slice (n + 1) (Array.length array) array)


{-| Shuffle sort order of input array without modifying its elements.
-}
shuffled : Array a -> Generator (Array (Maybe a))
shuffled elements =
    let
        len =
            Array.length elements

        randomIndex =
            Random.int 0 (len - 1)
                |> Random.map Just

        onNext maybeIndex =
            case maybeIndex of
                Nothing ->
                    Random.constant Array.empty

                Just n ->
                    let
                        rest =
                            exceptN n elements

                        picked =
                            Random.constant <|
                                Array.get n elements

                        genTails =
                            Random.lazy
                                (\_ ->
                                    if len == 1 then
                                        Random.constant Array.empty

                                    else
                                        shuffled rest
                                )
                    in
                    Random.map2 Array.push
                        picked
                        genTails
    in
    Random.andThen onNext randomIndex
