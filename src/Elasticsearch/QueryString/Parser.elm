module Elasticsearch.QueryString.Parser exposing
    (E(..), Range(..), RangeOp(..), parse)

import String
import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Infix exposing (..)
import Regex


-- Parser

type E
    = ETerm String
    | EPhrase String
    | EGroup (List E)
    | EPair (E, E)
    | EField String
    | ERegex String
    | EAnd E E
    | EOr E E
    | ENot E
    | ERange Range


type Range
    = Inclusive E E  -- [a TO b]
    | Exclusive E E  -- {a TO b}
    | LInclusive E E -- [a TO b}
    | RInclusive E E -- {a TO b]
    | SideUnbounded RangeOp E -- age:>10


type RangeOp
    = Gt
    | Gte
    | Lt
    | Lte


parse : String -> Result String (List E)
parse s =
    case Combine.parse program s of
    -- case Combine.parse (expr <* end) s of
        (Ok e, _) ->
            Ok e

        (Err ms, cx) ->
            Err ("parse error: " ++ (toString ms) ++ ", " ++ (toString cx))
            -- Err <| formatError s ms cx


program : Parser (List E)
program =
    let
        all acc cx =
            if String.isEmpty cx.input then
                (Ok (List.reverse acc), cx)
            else
                case app expr cx of
                    (Ok res', cx') ->
                        all (res' :: acc) cx'

                    (Err ms, cx') ->
                        (Err ms, cx')
    in
        primitive <| all []


atom : Parser E
atom =
    choice [ regex', phrase, term ]


parsers : Parser E
parsers =
    rec <| \() ->
        choice [ pair, range, group, atom ]


parsers' : Parser E
parsers' =
    (ws <| notOp) <*> parsers


expr : Parser E
expr =
    rec <| \() ->
        (ws <| choice [ parsers', parsers ]) `chainl` op


{- XXX: https://github.com/Bogdanp/elm-combine/issues/14
-}
subexpr : Parser E
subexpr =
    rec <| \() ->
        choice [ range, group, atom ]


whitespace : Parser String
whitespace =
    regex "[ \t\r\n]*" <?> "whitespace"


ws : Parser res -> Parser res
ws =
    between whitespace whitespace


quote : Parser String
quote =
    string "\""


quotes : Parser a -> Parser a
quotes e =
    quote *> e <* quote


{-|

Wildcards are included as part of Term for the time being.

    fox
    qu?ck bro*
-}
term : Parser E
term =
    ETerm
        <$> regex "[\\w\\d*?_-]+"
        <?> "term"


{-| "fox quick" -}
phrase : Parser E
phrase =
    EPhrase
        <$> quotes (regex "[\\ \\w\\d*?_-]+")
        <?> "phrase"

{-| (fox quick) -}
group : Parser E
group =
    rec <| \() ->
        EGroup
            <$> parens (many1 expr)
            <?> "group"


fieldSep : Parser String
fieldSep =
    string ":"


{-|
    status:active
    title:(quick OR brown)
    title:(quick brown)
    author:"John Smith"
    book.\*:(quick brown)
    field1:foo
-}
field : Parser E
field =
    EField
        <$> regex "[\\w_][\\w\\d_]*(\\\\.\\*)?" <* fieldSep
        <?> "field"


pair : Parser E
pair =
    rec <| \() ->
        EPair
            <$> ((,) `map` field `andMap` subexpr)
            <?> "pair"


{-|
    _missing_:title
-}
missing =
    string "_missing_"

{-|
    _exists_:title
-}
exists =
    string "_exists_"


slash : Parser String
slash =
    string "/"


slashes : Parser a -> Parser a
slashes e =
    slash *> e <* slash


lbrace : Parser String
lbrace =
    string "{"


rbrace : Parser String
rbrace =
    string "}"


lbracket : Parser String
lbracket =
    string "["


rbracket : Parser String
rbracket =
    string "]"



{-|
    name:/joh?n(ath[oa]n)/
-}
regex' : Parser E
regex' =
    ERegex
        <$> slashes (regex "[^\\/]+")
        <?> "regex"

{-|
    date:[2012-01-01 TO 2012-12-31]
    count:[1 TO 5]
    tag:{alpha TO omega}
    count:[10 TO *]
    date:{* TO 2012-01-01}
    count:[1 TO 5}
-}
range =
    rec <| \() ->
        ERange
            <$> (choice
                    [ inclusiveRange
                    , exclusiveRange
                    , lInclusiveRange
                    , rInclusiveRange
                    , sideUnboundedRange
                    ]
                )


inclusiveRange : Parser Range
inclusiveRange =
    rec <| \() ->
        brackets
            <| Inclusive `map` rangeLowerBound `andMap` rangeUpperBound
            <?> "[to]"


exclusiveRange : Parser Range
exclusiveRange =
    rec <| \() ->
        braces
            <| Exclusive `map` rangeLowerBound `andMap` rangeUpperBound
            <?> "{to}"


lInclusiveRange : Parser Range
lInclusiveRange =
    rec <| \() ->
        (between lbracket rbrace)
            <| LInclusive `map` rangeLowerBound `andMap` rangeUpperBound
            <?> "[to}"


rInclusiveRange : Parser Range
rInclusiveRange =
    rec <| \() ->
        (between lbrace rbracket)
            <| RInclusive `map` rangeLowerBound `andMap` rangeUpperBound
            <?> "{to]"


sideUnboundedRange : Parser Range
sideUnboundedRange =
    rec <| \() ->
        SideUnbounded `map` rangeOp `andMap` term
        <?> ">=<"


rangeOp : Parser RangeOp
rangeOp =
    choice [ gteOp, gtOp, lteOp, ltOp ]


gtOp : Parser RangeOp
gtOp =
    Gt <$ string ">"


gteOp : Parser RangeOp
gteOp =
    Gte <$ string ">="


ltOp : Parser RangeOp
ltOp =
    Lt <$ string "<"


lteOp : Parser RangeOp
lteOp =
    Lte <$ string "<="


rangeLowerBound =
    term <* rangeInf


rangeUpperBound =
    term


rangeInf : Parser String
rangeInf =
    ws <| string "TO"



{-|
    age:>10
    age:>=10
    age:<10
    age:<=10
    age:(>=10 AND <20)
    age:(+>=10 +<20)
-}
rangeSideBound =
    ""

{-|
    quick^2 fox
    "john smith"^2   (foo bar)^4
-}
boost =
    ""

{-|
    quick brown +fox -news
-}
conciseBool =
    ""



orOp : Parser (E -> E -> E)
orOp =
    EOr
        <$ string "OR"
        <?> "or"


andOp : Parser (E -> E -> E)
andOp =
    EAnd
        <$ string "AND"
        <?> "and"


notOp : Parser (E -> E)
notOp =
    ENot
        <$ string "NOT"
        <?> "not"


{-| NOT takes precedence over AND, which takes precedence over OR.

    (quick OR brown) AND fox
    status:(active OR pending) title:(full text search)^2
-}
op : Parser (E -> E -> E)
op =
    rec <| \() ->
        orOp `or` andOp


{-|
    quikc~ brwn~ foks~
    quikc~1
    "fox quick"~5
-}
proximity =
    ""

{-|
    The reserved characters are: + - = && || > < ! ( ) { } [ ] ^ " ~ * ? : \ /
-}
reservedList : List String
reservedList =
    [ "+"
    , "-"
    , "="
    , "&"
    , "|"
    , ">"
    , "<"
    , "!"
    , "("
    , ")"
    , "{"
    , "}"
    , "["
    , "]"
    , "^"
    , "\""
    , "~"
    , "*"
    , "?"
    , ":"
    , "\\"
    , "/"
    ]


reserved : List String -> Parser String
reserved xs =
    regex <| "[" ++ (String.join "" (List.map Regex.escape xs)) ++ "]+"


notReserved : List String -> Parser String
notReserved xs =
    regex <| "[^" ++ (String.join "" (List.map Regex.escape xs)) ++ "]+"
