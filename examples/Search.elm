module Search exposing (..)

import Elasticsearch.Search as Search exposing (Search)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
    exposing
        ( Decoder
        , field
        , string
        , succeed
        )
import Json.Decode.Extra exposing ((|:))
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
    | Success (List Tweet)
    | Failure String


type alias Tweet =
    { user : String
    , postDate : String
    , message : String
    }


tweetDecode : Decoder Tweet
tweetDecode =
    succeed Tweet
        |: field "user" string
        |: field "postDate" string
        |: field "message" string


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
    | Fetch (Result Http.Error (Search Tweet))


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
                Ok value ->
                    { model | hits = Success (fromSearch value) } ! []

                Err error ->
                    { model | hits = Failure (toString error) } ! []


fromSearch : Search a -> List a
fromSearch res =
    List.map .source res.hits.hits


withDefault : String -> String -> String
withDefault default query =
    if String.isEmpty (String.trim query) then
        default
    else
        query


get : String -> String -> Cmd Msg
get url query =
    Search.fetch url [ Search.Query (withDefault "*" query) ] tweetDecode
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
            , button [ onClick Submit, type_ "submit" ] [ text "Search" ]
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
