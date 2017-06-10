module Parser exposing (..)

import Elasticsearch.QueryString.Parser as QS exposing (E(..), Range(..), RangeOp(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main : Program Never Model Msg
main =
    Html.program
        { init = model ! []
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Color =
    ( Int, Int, Int )


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
        , "quick brown +fox -news"
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
        , "\"john smith\"^2 (foo bar)^4"
        , "(quick OR brown) AND fox"
        , "status:(active OR pending) title:(full text search)^2"
        , "quikc~ brwn~ foks~"
        , "quikc~1"
        , "\"fox quick\"~5"
        , "/joh?n(ath[oa]n)/^8"
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
        [ h1 [] [ text "Example: Query string parser" ]
        , input
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


asItem : String -> Html Msg
asItem x =
    li
        [ style [ ( "padding", "1rem" ) ] ]
        [ div
            []
            [ code
                [ style
                    [ ( "background-color", "rgb(230, 230, 230)" )
                    , ( "padding", "0.4rem" )
                    , ( "display", "inline-block" )
                    ]
                ]
                [ text x ]
            ]
        , astView x
        ]


astView : String -> Html Msg
astView content =
    case QS.parse content of
        Nothing ->
            p [] [ text "Type above your query" ]

        Just res ->
            case res of
                Err error ->
                    p [] [ text error ]

                Ok ast ->
                    div [] (List.map (expression 1) ast)


item : Html Msg -> Html Msg
item x =
    li [] [ x ]


expression : Int -> E -> Html Msg
expression depth e =
    case e of
        ETerm s f b ->
            term s f b

        EPhrase s p b ->
            phrase s p b

        EGroup xs b ->
            let
                ys =
                    List.map (expression (depth + 1)) xs
            in
            group_ depth (text "(" :: ys ++ (text ")" :: boostToView b))

        EPair ( f, t ) ->
            group ( 200, 230, 255 ) [ expression 0 f, expression (depth + 1) t ]

        EField s ->
            span [] [ text (s ++ ":") ]

        ERegex s b ->
            regex s b

        EAnd a b ->
            group_
                depth
                (expression (depth + 1) a :: [ op "AND", expression (depth + 1) b ])

        EOr a b ->
            group_
                depth
                (expression (depth + 1) a :: [ op "OR", expression (depth + 1) b ])

        ENot e ->
            group_
                depth
                (op "NOT" :: [ expression (depth + 1) e ])

        EMust e ->
            group_
                depth
                (op "+" :: [ expression (depth + 1) e ])

        EMustNot e ->
            group_
                depth
                (op "-" :: [ expression (depth + 1) e ])

        ERange range ->
            let
                rng ( l, u ) a b =
                    group_
                        depth
                        (text l
                            :: [ expression (depth + 1) a ]
                            ++ [ op "TO", expression (depth + 1) b, text u ]
                        )
            in
            case range of
                Inclusive a b ->
                    rng ( "[", "]" ) a b

                Exclusive a b ->
                    rng ( "{", "}" ) a b

                LInclusive a b ->
                    rng ( "[", "}" ) a b

                RInclusive a b ->
                    rng ( "{", "]" ) a b

                SideUnbounded rop a ->
                    let
                        op_ =
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
                    group_
                        depth
                        [ op op_, expression (depth + 1) a ]



-- _ ->
--     token (255, 200, 150) (toString e)


term : String -> QS.Fuzziness -> QS.Boost -> Html Msg
term s f b =
    wrapper
        ( 120, 180, 255 )
        (token ( 150, 200, 255 ) s
            :: fuzzinessToView f
            ++ boostToView b
        )


fuzzinessToView : QS.Fuzziness -> List (Html Msg)
fuzzinessToView x =
    Maybe.withDefault emptyText (Maybe.map fuzzinessView x)


fuzzinessView : a -> List (Html Msg)
fuzzinessView x =
    [ op "~", text (toString x) ]


boostToView : QS.Boost -> List (Html Msg)
boostToView x =
    Maybe.withDefault emptyText (Maybe.map boostView x)


boostView : a -> List (Html Msg)
boostView x =
    [ op "^", text (toString x) ]


phrase : String -> QS.Proximity -> QS.Boost -> Html Msg
phrase s p b =
    wrapper
        ( 150, 220, 100 )
        (token ( 175, 250, 150 ) ("\"" ++ s ++ "\"")
            :: proximityToView p
            ++ boostToView b
        )


regex : String -> QS.Boost -> Html Msg
regex s b =
    wrapper
        ( 255, 220, 120 )
        (token ( 255, 250, 150 ) ("/" ++ s ++ "/")
            :: boostToView b
        )


proximityToView : QS.Proximity -> List (Html Msg)
proximityToView x =
    Maybe.withDefault emptyText (Maybe.map proximityView x)


proximityView : a -> List (Html Msg)
proximityView x =
    [ op "~", text (toString x) ]


emptyText : List (Html Msg)
emptyText =
    [ text "" ]


token : Color -> String -> Html Msg
token color s =
    span
        [ style
            [ ( "background-color", "rgb" ++ toString color )
            , ( "display", "inline-block" )
            , ( "padding", "2px" )
            , ( "margin", "2px" )
            ]
        ]
        [ text s ]


group : Color -> List (Html Msg) -> Html Msg
group color xs =
    span
        [ style
            [ ( "background-color", "rgb" ++ toString color )
            , ( "display", "inline-block" )
            , ( "padding", "4px" )
            , ( "margin", "2px" )
            ]
        ]
        xs


group_ : Int -> List (Html Msg) -> Html Msg
group_ depth xs =
    let
        color =
            255 - (depth * 10)
    in
    group ( color, color, color ) xs


op : String -> Html Msg
op s =
    span
        [ style
            [ ( "background-color", "rgb(250, 185, 210)" )
            , ( "display", "inline-block" )
            , ( "padding", "2px" )
            , ( "margin", "2px" )
            , ( "border-radius", "3px" )
            , ( "font-size", "0.8rem" )
            ]
        ]
        [ text s ]


wrapper : Color -> List (Html Msg) -> Html Msg
wrapper color xs =
    span
        [ style
            [ ( "background-color", "rgb" ++ toString color )
            , ( "display", "inline-block" )
            , ( "margin", "2px" )
            ]
        ]
        xs
