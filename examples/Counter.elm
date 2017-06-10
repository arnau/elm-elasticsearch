module Counter exposing (..)

import Elasticsearch.Count as Count exposing (Count)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import String


main : Program Never Model Msg
main =
    Html.program
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


model : Model
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
    | Fetch (Result Http.Error Count)


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
            , get model.endpoint model.query
            )

        Fetch result ->
            case result of
                Ok { count } ->
                    { model | hits = Success count } ! []

                Err error ->
                    { model | hits = Failure (toString error) } ! []


withDefault : String -> String -> String
withDefault default query =
    if String.isEmpty (String.trim query) then
        default
    else
        query


get : String -> String -> Cmd Msg
get url query =
    Count.fetch url [ Count.Query (withDefault "*" query) ]
        |> Http.send Fetch



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ fieldset
            []
            [ input
                [ onInput Add, value model.query ]
                []
            , button [ onClick Submit, type_ "submit" ] [ text "Count" ]
            ]
        , p [] [ text <| hitsToString model.hits ]
        ]


hitsToString : Hits -> String
hitsToString hits =
    case hits of
        Await ->
            "waiting"

        Failure e ->
            e

        Success x ->
            toString x
