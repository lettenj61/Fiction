port module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Fiction.Token as Token exposing (Analyzer, Entry, Token)
import Fiction.Translate as Translate exposing (Mapping)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Task



-- MAIN


main : Program (Maybe (List String)) Model Msg
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
    , analyzer : Analyzer
    , reps : Mapping
    , inputText : String
    , translated : String
    }


init : Maybe (List String) -> ( Model, Cmd Msg )
init maybeSamples =
    ( { samples = Maybe.withDefault [] maybeSamples
      , analyzer = initAnalyzer maybeSamples
      , reps = Dict.empty
      , inputText = ""
      , translated = ""
      }
    , Task.perform (\_ -> NewMapping) <| Task.succeed never
    )


initAnalyzer : Maybe (List String) -> Analyzer
initAnalyzer maybeSamples =
    let
        newAnalyzer =
            { tokens = Dict.empty }
    in
    maybeSamples
        |> Maybe.map (\src -> Token.analyzeMore src newAnalyzer)
        |> Maybe.withDefault newAnalyzer



-- UPDATE


type Msg
    = NoOp
    | NewMapping
    | GotMapping Mapping
    | GotText String
    | SetTranslation String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewMapping ->
            ( model
            , Random.generate GotMapping <|
                Translate.generateMapping model.analyzer
            )

        GotMapping dict ->
            ( { model | reps = dict }
            , Cmd.none
            )

        GotText newText ->
            ( { model | inputText = newText }
            , Cmd.none
            )

        SetTranslation src ->
            let
                translated =
                    Translate.translate model.reps model.inputText
            in
            ( { model | translated = translated }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    section
        [ class "elm-main" ]
        [ h1 [ class "title" ] [ text "FICTION" ]
        , button [ onClick NewMapping ] [ text "generate" ]
        , div [] (viewControls model)
        , viewTokens model
        ]


viewControls : Model -> List (Html Msg)
viewControls model =
    [ input
        [ type_ "text"
        , onInput GotText
        ]
        []
    , button
        [ onClick <| SetTranslation model.inputText ]
        [ text "translate" ]
    , p [] [ text <| model.translated ]
    ]


viewTokens : Model -> Html msg
viewTokens model =
    let
        tokens =
            Dict.values model.analyzer.tokens
                |> List.sortBy .count
                |> List.take 0
    in
    ul [] <|
        List.map
            (\tok -> li [] [ viewToken tok ])
            tokens


viewToken : Entry -> Html msg
viewToken entry =
    let
        showChain links =
            List.map Token.toString links
                |> String.join ", "
    in
    article
        [ class "token-card" ]
        [ div
            [ class "card-title" ]
            [ p [] [ text <| Token.toString entry.token ] ]
        , div
            [ class "card-body" ]
            [ div
                []
                [ p [] [{- text <| showChain entry.chain.prevs -}]
                , p [] [{- text <| showChain entry.chain.nexts -}]
                ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
