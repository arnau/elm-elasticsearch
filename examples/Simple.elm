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
import Elasticsearch.QueryString.Parser as QS exposing
    (E(..), Range(..), RangeOp(..))


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
    , showcase : List String
    }


model : Model
model =
    -- { content = "((quick AND fox) OR (brown AND fox) OR fox) AND NOT news" }
    { content = "((quick AND is:fox) OR (/bro.?n/ AND \"fox trot\") OR date:[2016-10-15 TO 2016-10-20]) AND NOT news tag:{a TO s}"
    , showcase =
        [ "((quick AND fox) OR (brown AND fox) OR fox) AND NOT news"
        , "fox"
        , "qu?ck bro*"
        , "\"fox quick\""
        , "(fox quick)"
        , "status:active"
        , "title:(quick OR brown)"
        , "title:(quick brown)"
        , "author:\"John Smith\""
        -- , "book.\\*:(quick brown)"
        , "field1:foo"
        , "_missing_:title"
        , "_exists_:title"
        , "name:/joh?n(ath[oa]n)/"
        , "date:[2012-01-01 TO 2012-12-31]"
        , "count:[1 TO 5]"
        , "tag:{alpha TO omega}"
        , "count:[10 TO *]"
        , "date:{* TO 2012-01-01}"
        , "count:[1 TO 5}"
        , "age:>10"
        , "age:>=10"
        , "age:<10"
        , "age:<=10"
        , "quick^2 fox"
        -- , "\"john smith\"^2 (foo bar)^4"
        , "(quick OR brown) AND fox"
        -- , "status:(active OR pending) title:(full text search)^2"
        , "quikc~ brwn~ foks~"
        , "quikc~1"
        , "\"fox quick\"~5"
        ]
    }



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
        , div
            []
            [ h2 [] [ text "Showcase" ]
            , ul [] (List.map asItem model.showcase)
            ]
        ]

asItem x =
    li [] [ astView x ]


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
        ETerm s f b ->
            term s f b

        EPhrase s p b ->
            phrase s p b

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
            group'
                depth
                ((expression (depth + 1) a) :: [ (op "AND"), (expression (depth + 1) b) ])

        EOr a b ->
            group
                depth
                ((expression (depth + 1) a) :: [ (op "OR"), (expression (depth + 1) b) ])

        ENot e ->
            group'
                depth
                ((op "NOT") :: [ (expression (depth + 1) e) ])

        ERange range ->
            let
                rng (l, u) a b =
                    group'
                        depth
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

                    LInclusive a b ->
                        rng ("[", "}") a b

                    RInclusive a b ->
                        rng ("{", "]") a b

                    SideUnbounded rop a ->
                        let
                            op' =
                                case rop of
                                    Gt ->
                                        ">"

                                    Gte ->
                                        ">="

                                    Lt ->
                                        "<"

                                    Lte ->
                                        "<="
                        in
                            group'
                                depth
                                [ (op op'), (expression (depth + 1) a) ]

        -- _ ->
        --     token (255, 200, 150) (toString e)


term s f b =
    wrapper
        (120, 180, 255)
        ((token (150, 200, 255) s)
            :: (fuzzinessToView f) ++ (boostToView b))


fuzzinessToView x =
    Maybe.withDefault emptyText (Maybe.map fuzzinessView x)


fuzzinessView x =
    [ (op "~"), text (toString x) ]


boostToView x =
    Maybe.withDefault emptyText (Maybe.map boostView x)


boostView x =
    [ (op "^"), text (toString x) ]

phrase s p b =
    wrapper
        (150, 220, 100)
        ((token (175, 250, 150) s)
            :: (proximityToView p) ++ (boostToView b))


proximityToView x =
    Maybe.withDefault emptyText (Maybe.map proximityView x)


proximityView x =
    [ (op "~"), text (toString x) ]


emptyText =
    [ (text "") ]


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

group' depth xs =
    let
        color =
            255 - (depth * 10)
    in
        group (color, color, color) xs

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


wrapper color xs =
    span
        [ style [ ( "background-color", "rgb" ++ toString color )
                , ( "display", "inline-block" )
                , ( "margin", "2px" )
                ] ]
        xs


