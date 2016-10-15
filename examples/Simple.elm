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
import Elasticsearch.QueryString.Parser as QS exposing (E(..), Range(..))


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
    -- { content = "((quick AND fox) OR (brown AND fox) OR fox) AND NOT news" }
    { content = "((quick AND is:fox) OR (/bro.?n/ AND \"fox trot\") OR date:[2016-10-15 TO 2016-10-20]) AND NOT news tag:{a TO s}" }



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
        , astView model.content
        ]


astView content =
    case QS.parse content of
        Err error ->
            p [] [ text error ]

        Ok ast ->
            div [] (List.map (expression 1) ast)



item x =
    li [] [ x ]


item' x xs =
    if List.isEmpty xs then
        item x
    else
        li
            []
            [ text (toString x)
            , ul [] [ text (toString xs) ]
            ]


expression depth e =
    case e of
        ETerm s ->
            token (150, 200, 255) s

        EPhrase s ->
            token (175, 250, 150) ("\"" ++ s ++ "\"")

        EGroup xs ->
            let
                color =
                    255 - (depth * 10)
                ys =
                    List.map (expression (depth + 1)) xs
            in
                group (color, color, color) ((text "(") :: ys ++ [(text ")")])

        EPair (f, t) ->
            group (200, 230, 255) [ expression 0 f, expression (depth + 1) t ]

        EField s ->
            span [] [ text (s ++ ":") ]

        ERegex s ->
            token (255, 250, 150) ("/" ++ s ++ "/")

        EAnd a b ->
            let
                color =
                    255 - (depth * 10)
            in
                group
                    (color, color, color)
                    ((expression (depth + 1) a) :: [ (op "AND"), (expression (depth + 1) b) ])

        EOr a b ->
            let
                color =
                    255 - (depth * 10)
            in
                group
                    (color, color, color)
                    ((expression (depth + 1) a) :: [ (op "OR"), (expression (depth + 1) b) ])

        ENot e ->
            let
                color =
                    255 - (depth * 10)
            in
                group
                    (color, color, color)
                    ((op "NOT") :: [ (expression (depth + 1) e) ])

        ERange range ->
            let
                color =
                    255 - (depth * 10)

                rng (l, u) a b =
                    group
                        (color, color, color)
                        (
                            (text l)
                            :: [ (expression (depth + 1) a) ]
                            ++ [ (op "TO"), (expression (depth + 1) b), (text u)]
                        )

            in
                case range of
                    Inclusive a b ->
                        rng ("[", "]") a b

                    Exclusive a b ->
                        rng ("{", "}") a b

                    _ ->
                        group
                            (color, color, color)
                            [ token (255, 200, 150) (toString range) ]


token color s =
    span
        [ style [ ( "background-color", "rgb" ++ toString color )
                , ( "display", "inline-block" )
                , ( "padding", "2px" )
                , ( "margin", "2px" )
                ] ]
        [ text s ]


group color xs =
    span
        [ style [ ( "background-color", "rgb" ++ toString color )
                , ( "display", "inline-block" )
                , ( "padding", "4px" )
                , ( "margin", "2px" )
                ] ]
        xs

op s =
    span
        [ style [ ( "background-color", "rgb(250, 185, 210)" )
                , ( "display", "inline-block" )
                , ( "padding", "2px" )
                , ( "margin", "2px" )
                , ( "border-radius", "3px" )
                , ( "font-size", "0.8rem" )
                ] ]
        [ text s ]


