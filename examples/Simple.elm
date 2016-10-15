module Simple exposing (..)

import Dom
import Html.App as App
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String
import Task
import Combine exposing (..)
import Combine.Infix exposing (..)
import Combine.Num exposing (int)
import Elasticsearch.QueryString.Parser as QS


main =
    App.program
        { init = model ! []
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { content : String
    }


model : Model
model =
    { content = "" }



-- UPDATE


type Msg
    = NoOp
    | Change String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Change content ->
            { model | content = content } ! []



-- VIEW


view : Model -> Html Msg
view model =
    div [ style [ ( "padding", "1rem" ) ] ]
        [ input
            [ onInput Change
            , value model.content
            , style [ ( "font-size", "1.5rem" ), ( "width", "100%" ) ]
            ]
            []
        , p [] [ text (qsToString model.content) ]
        ]


qsToString content =
    case QS.parse content of
        Err error ->
            error

        Ok a ->
            toString a
