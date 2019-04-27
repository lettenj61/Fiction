module Fiction.Token exposing (Token, tokenize, fromString)

import Regex as Regex

-- TYPES


{- Record which tracks appearance of a token
-}
type alias Freq =
    { head : Int
    , middle : Int
    , last : Int
    }


{- A token in the app
-}
type Token = Token String


type alias Link = (Token, Int)


type alias Chain =
    { leading : List Link
    , trailing : List Link
    }



-- HELPERS


fromString : String -> Token
fromString = Token


read : Token -> String
read (Token repr) = repr


tokenizer : Regex.Regex
tokenizer =
    Maybe.withDefault Regex.never <|
        Regex.fromString "[aeiouy]+|[^aeiouy]+"


tokenize : String -> List String
tokenize raw =
    let
        extract { match } = match
    in
    Regex.find tokenizer raw
        |> List.map extract