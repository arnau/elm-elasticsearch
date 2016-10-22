module Counter exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import String
import Task
import Elasticsearch.Count as Count exposing (Count)


main =
    App.program
        { init = model ! []
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


-- MODEL


type alias Model =
    { endpoint : String
    , query : String
    , hits : Hits
    }


type Hits
    = Await
    | Success Int
    | Failure String


model =
    { endpoint = "http://localhost:9292"
    , query = ""
    , hits = Await
    }

-- UPDATE


type Msg
    = NoOp
    | Add String
    | Submit
    | FetchSuccess Count
    | FetchFailure Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- XXX: case Debug.log "filter" msg of
    case msg of
        NoOp ->
            model ! []

        Add query ->
            { model | query = query } ! []

        Submit ->
            ( model
            , count' model.endpoint model.query
            )

        FetchSuccess { count } ->
            { model | hits = Success count } ! []

        FetchFailure error ->
            { model | hits = Failure (toString error) } ! []


withDefault : String -> String -> String
withDefault default query =
    if String.isEmpty (String.trim query) then
        default
    else
        query


count' : String -> String -> Cmd Msg
count' url query =
    Count.fetch url [ Count.Query (withDefault "*" query) ]
        |> Task.perform FetchFailure FetchSuccess


-- VIEW


-- view : Model -> Html Msg
view model =
    div
        []
        [ fieldset
            []
            [ input
                [ onInput Add, value model.query ]
                []
            , button [ onClick Submit, type' "submit" ] [ text "Count" ]
            ]
        , p [] [ text <| hitsToString model.hits ]
        ]


hitsToString hits =
    case hits of
        Await ->
            "waiting"

        Failure e ->
            e

        Success x ->
            toString x
