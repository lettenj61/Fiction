module Fiction.Token exposing
    ( Analyzer
    , Entry
    , Token
    , TokenKind(..)
    , analyze
    , analyzeMore
    , filterCandidate
    , fromString
    , isSingleVowel
    , isSingleVowel_
    , isVowel
    , toString
    , tokenize
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Regex as Regex
import Set exposing (Set)



-- TYPES


type alias Freq =
    { head : Int
    , middle : Int
    , last : Int
    }


type TokenKind
    = Vowel
    | Consonant


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


get : String -> Analyzer -> Maybe Entry
get key { tokens } =
    Dict.get key tokens



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


isSingleVowel : Token -> Bool
isSingleVowel (Token repr) =
    isSingleVowel_ repr


isSingleVowel_ : String -> Bool
isSingleVowel_ tok =
    String.length tok
        == 1
        && String.all (\c -> Set.member c vowels) tok


testEntry :
    (Entry -> Entry -> Bool)
    -> Analyzer
    -> String
    -> String
    -> Bool
testEntry pred analyzer tok1 tok2 =
    let
        e1 =
            get tok1 analyzer

        e2 =
            get tok2 analyzer
    in
    case ( e1, e2 ) of
        ( Just l, Just r ) ->
            pred l r

        _ ->
            False


hasSameKinds : Analyzer -> String -> String -> Bool
hasSameKinds =
    let
        areBothVowels e1 e2 =
            let
                t1 =
                    e1.token

                t2 =
                    e2.token
            in
            isVowel t1 == isVowel t2
    in
    testEntry areBothVowels


hasCloserFreq : Analyzer -> String -> String -> Bool
hasCloserFreq =
    let
        samePositions e1 e2 =
            let
                fa =
                    e1.freq

                fb =
                    e2.freq
            in
            (fa.head > 0)
                == (fb.head > 0)
                && (fa.last > 0)
                == (fb.last > 0)
    in
    testEntry samePositions


areBothUniq : Analyzer -> String -> String -> Bool
areBothUniq =
    let
        compare_ e1 e2 =
            let
                aCount =
                    e1.count

                bCount =
                    e2.count
            in
            (aCount == 1) == (bCount == 1)
    in
    testEntry compare_


filterCandidate : Analyzer -> String -> String -> Bool
filterCandidate data tok =
    \that ->
        not (tok == that)
            && not (isSingleVowel_ tok)
            && areBothUniq data tok that
            && hasSameKinds data tok that
            && hasCloserFreq data tok that



-- CONSTANTS


vowels : Set Char
vowels =
    Set.fromList [ 'a', 'e', 'i', 'o', 'u', 'y' ]
