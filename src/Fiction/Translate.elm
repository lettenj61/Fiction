module Fiction.Translate exposing
    ( Mapping
    , Translator(..)
    , TranslatorConfig
    , generateMapping
    , translate
    )

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



-- HELPERS


translate : Mapping -> String -> String
translate reps content =
    let
        replace tok =
            Dict.get tok reps
                |> Maybe.withDefault tok

        changeWord word =
            Token.tokenize word
                |> List.map (Token.toString >> replace)
                |> String.join ""
    in
    String.words content
        |> List.map changeWord
        |> String.join " "


generateMapping : Analyzer -> Generator Mapping
generateMapping data =
    let
        initialKeys =
            Dict.keys data.tokens

        regularMapping =
            generateRegularMapping initialKeys data
                |> Random.map Dict.fromList
    in
    Random.map2 Dict.union
        singleVowelMapping
        regularMapping


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
        (arrayShuffle <| Array.fromList chars)


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


shuffle : List a -> Generator (List a)
shuffle source =
    let
        catMaybes maybes =
            Array.toList maybes
                |> List.filterMap identity
    in
    arrayShuffle (Array.fromList source)
        |> Random.map catMaybes


pick1 : Array a -> Generator (Maybe a)
pick1 elements =
    let
        nextIndex =
            Random.int 0 <| Array.length elements - 1
    in
    Random.map (\n -> Array.get n elements) nextIndex


{-| Shuffle sort order of input array without modifying its elements.
-}
arrayShuffle : Array a -> Generator (Array (Maybe a))
arrayShuffle elements =
    arrayShuffleWith identity elements


arrayShuffleWith : (Array a -> Array a) -> Array a -> Generator (Array (Maybe a))
arrayShuffleWith transform elements =
    let
        actualElements =
            transform elements

        len =
            Array.length actualElements

        onNext index =
            let
                rest =
                    exceptN index actualElements

                picked =
                    Random.constant <|
                        Array.get index actualElements

                genTail =
                    Random.lazy
                        (\_ ->
                            if len == 1 then
                                Random.constant Array.empty

                            else
                                arrayShuffleWith transform rest
                        )
            in
            Random.map2 Array.push
                picked
                genTail
    in
    Random.andThen onNext (Random.int 0 (len - 1))


generateRegularMapping :
    List String
    -> Analyzer
    -> Generator (List ( String, String ))
generateRegularMapping keys data =
    let
        keysIndexed =
            Array.fromList keys

        genItem =
            pick1 keysIndexed

        genMore maybeToken =
            case maybeToken of
                Nothing ->
                    Random.constant []

                Just tok ->
                    let
                        candidates =
                            Array.filter
                                (\that -> Token.filterCandidate data tok that)
                                keysIndexed

                        genRep =
                            pick1 candidates
                                |> Random.map (Maybe.withDefault tok)

                        genPair =
                            Random.map2 Tuple.pair
                                genRep
                                (Random.constant tok)

                        newKeys =
                            List.filter (\k -> k /= tok) keys
                    in
                    Random.map2 (::) genPair <|
                        Random.lazy
                            (\_ ->
                                generateRegularMapping newKeys data
                            )
    in
    Random.andThen
        genMore
        genItem
