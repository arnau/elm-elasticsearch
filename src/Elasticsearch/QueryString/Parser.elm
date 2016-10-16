module Elasticsearch.QueryString.Parser exposing
    (E(..), Range(..), RangeOp(..), parse)

import String
import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Infix exposing (..)
import Combine.Num exposing (int)
import Regex


-- Parser

type E
    = ETerm String Fuzziness Boost
    | EPhrase String
    | EGroup (List E)
    | EPair (E, E)
    | EField String
    | ERegex String
    | EAnd E E
    | EOr E E
    | ENot E
    | ERange Range


type alias Fuzziness =
    Maybe Int


type alias Boost =
    Maybe Int


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
        <*> fuzziness
        <*> boost
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


{-| Inclusive range

    date:[2012-01-01 TO 2012-12-31]
    count:[1 TO 5]
-}
inclusiveRange : Parser Range
inclusiveRange =
    rec <| \() ->
        brackets
            <| Inclusive `map` rangeLowerBound `andMap` rangeUpperBound
            <?> "[to]"


{-| Exclusive range

    {1 TO 5}
-}
exclusiveRange : Parser Range
exclusiveRange =
    rec <| \() ->
        braces
            <| Exclusive `map` rangeLowerBound `andMap` rangeUpperBound
            <?> "{to}"


{-| Left inclusive range

    [1 TO 5}
-}
lInclusiveRange : Parser Range
lInclusiveRange =
    rec <| \() ->
        (between lbracket rbrace)
            <| LInclusive `map` rangeLowerBound `andMap` rangeUpperBound
            <?> "[to}"


{-| Right inclusive range

    {1 TO 5]
-}
rInclusiveRange : Parser Range
rInclusiveRange =
    rec <| \() ->
        (between lbrace rbracket)
            <| RInclusive `map` rangeLowerBound `andMap` rangeUpperBound
            <?> "{to]"


{-| Side unbounded range

    age:>10
    age:>=10
    age:<10
    age:<=10
    age:(>=10 AND <20)
    age:(+>=10 +<20)
-}
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



{-| Fuzziness with default edit distance of 2

    quikc~ brwn~ foks~
    quikc~1
-}
fuzziness : Parser Fuzziness
fuzziness =
    maybe ((string "~") *> (int `or` (succeed 2)))


{-|
    "fox quick"~5
-}
-- proximity : Parser Modifier
proximity =
    ""
--     Proximity
--         <$> ((string "~") *> int)


{-|
    quick^2 fox
    "john smith"^2   (foo bar)^4
-}
boost : Parser Boost
boost =
    maybe ((string "^") *> int)


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
