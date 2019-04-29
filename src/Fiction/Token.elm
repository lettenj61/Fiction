module Fiction.Token exposing
    ( Analyzer
    , Token
    , analyze
    , fromString
    , isVowel
    , toString
    , tokenize
    )

import Array as Array exposing (Array)
import Dict as Dict exposing (Dict)
import Regex as Regex
import Set as Set exposing (Set)
import String as String



-- TYPES


type alias Freq =
    { head : Int
    , middle : Int
    , last : Int
    }


type Token
    = Token String


type alias Chain =
    { prevs : List Token
    , nexts : List Token
    }


type alias Entry =
    { count : Int
    , chain : Chain
    , freq : Freq
    , token : Token
    }


type alias Analyzer =
    { tokens : Dict String Entry
    }



-- PUBLIC API


analyze : String -> Analyzer
analyze content =
    analyzeMore
        (String.words content)
        { tokens = Dict.empty }



-- HELPERS


analyzeMore : List String -> Analyzer -> Analyzer
analyzeMore words analyzer =
    let
        onWord word memo =
            let
                tokens =
                    Array.fromList <| tokenize word
            in
            updateAnalyzer tokens memo
    in
    List.foldl onWord analyzer words


updateAnalyzer : Array Token -> Analyzer -> Analyzer
updateAnalyzer tokens analyzer =
    let
        len =
            Array.length tokens

        collect ( i, tok ) analyzer_ =
            let
                setFreq =
                    updateFreqs tok i (len - 1) analyzer_

                setChain =
                    updateChain tok i tokens

                setCount entry =
                    { entry | count = entry.count + 1 }

                updateAll maybeEntry =
                    let
                        entry =
                            Maybe.withDefault
                                (newEntry tok)
                                maybeEntry
                    in
                    Just <| (setCount >> setFreq >> setChain) entry
            in
            { analyzer_
                | tokens =
                    Dict.update
                        (toString tok)
                        updateAll
                        analyzer_.tokens
            }

        indexed =
            Array.indexedMap Tuple.pair tokens
    in
    Array.foldl collect analyzer indexed


updateFreqs :
    Token
    -> Int
    -> Int
    -> Analyzer
    -> Entry
    -> Entry
updateFreqs (Token t) pos lastPos { tokens } =
    let
        freq =
            case Dict.get t tokens of
                Just entry ->
                    entry.freq

                Nothing ->
                    zeroFreq

        newFreq =
            if pos == 0 then
                { freq | head = freq.head + 1 }

            else if pos == lastPos then
                { freq | last = freq.last + 1 }

            else
                { freq | middle = freq.middle + 1 }
    in
    \entry -> { entry | freq = newFreq }


updateChain :
    Token
    -> Int
    -> Array Token
    -> Entry
    -> Entry
updateChain (Token t) idx elems =
    let
        prev =
            Array.get (idx - 1) elems

        next =
            Array.get (idx + 1) elems

        feed chars isPrev body =
            if List.member chars body then
                body

            else if isPrev then
                chars :: body

            else
                body ++ [ chars ]
    in
    \entry ->
        case ( prev, next ) of
            ( Just p, Just n ) ->
                { entry
                    | chain =
                        { prevs = feed p True entry.chain.prevs
                        , nexts = feed n False entry.chain.nexts
                        }
                }

            ( Just p, Nothing ) ->
                { entry
                    | chain =
                        { prevs = feed p True entry.chain.prevs
                        , nexts = entry.chain.nexts
                        }
                }

            ( Nothing, Just n ) ->
                { entry
                    | chain =
                        { prevs = entry.chain.prevs
                        , nexts = feed n False entry.chain.nexts
                        }
                }

            ( _, _ ) ->
                entry


emptyChain : Chain
emptyChain =
    { prevs = [], nexts = [] }


zeroFreq : Freq
zeroFreq =
    { head = 0, middle = 0, last = 0 }


newEntry : Token -> Entry
newEntry token =
    { count = 0
    , chain = emptyChain
    , freq = zeroFreq
    , token = token
    }


fromString : String -> Token
fromString =
    Token


toString : Token -> String
toString (Token repr) =
    repr


tokenizer : Regex.Regex
tokenizer =
    Maybe.withDefault Regex.never <|
        Regex.fromString "[aeiouy]+|[^aeiouy]+"


tokenize : String -> List Token
tokenize raw =
    let
        extract { match } =
            fromString match
    in
    Regex.find tokenizer raw
        |> List.map extract


isVowel : Token -> Bool
isVowel (Token repr) =
    String.all (\c -> Set.member c vowels) repr


hasCloserFreq : String -> String -> Dict String Freq -> Bool
hasCloserFreq t1 t2 freqs =
    let
        f1 =
            Dict.get t1 freqs

        f2 =
            Dict.get t2 freqs

        similar fa fb =
            (fa.head > 0)
                == (fb.head > 0)
                && (fa.last > 0)
                == (fb.last > 0)
    in
    case ( f1, f2 ) of
        ( Just l, Just r ) ->
            similar l r

        _ ->
            False



-- CONSTANTS


vowels : Set Char
vowels =
    Set.fromList [ 'a', 'e', 'i', 'o', 'u', 'y' ]
