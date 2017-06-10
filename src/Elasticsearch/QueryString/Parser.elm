module Elasticsearch.QueryString.Parser
    exposing
        ( Boost
        , E(..)
        , Fuzziness
        , Proximity
        , Range(..)
        , RangeOp(..)
        , parse
        )

{-| This module exposes the tools for converting an Elasticsearch query string
into an AST.

@docs parse


# AST Types

@docs E, Range, RangeOp


## Modifiers

Modifiers are specific for different types of expression. For example, a
`ETerm` expects `Fuzziness` and `Boost` but `ERegex` only allows `Boost`.

@docs Fuzziness, Boost, Proximity

-}

import Combine exposing (..)
import Combine.Num exposing (float, int)
import Regex
import String


{-| The AST expressions.
-}
type E
    = ETerm String Fuzziness Boost
    | EPhrase String Proximity Boost
    | EGroup (List E) Boost
    | ERegex String Boost
    | EPair ( E, E )
    | EField String
    | EAnd E E
    | EOr E E
    | ENot E
    | EMust E
    | EMustNot E
    | ERange Range


{-| -}
type alias Fuzziness =
    Maybe Int


{-| -}
type alias Boost =
    Maybe Float


{-| -}
type alias Proximity =
    Maybe Int


{-| -}
type Range
    = Inclusive E E -- [a TO b]
    | Exclusive E E -- {a TO b}
    | LInclusive E E -- [a TO b}
    | RInclusive E E -- {a TO b]
    | SideUnbounded RangeOp E -- age:>10


{-| -}
type RangeOp
    = Gt
    | Gte
    | Lt
    | Lte


{-| Takes a string and returns the equivalent AST.

If the string is empty it returns `Nothing` so you can handle the empty query
case yourself.

-}
parse : String -> Maybe (Result String (List E))
parse s =
    -- Don't bother parsing anything if the initial string is empty.
    if String.isEmpty s then
        Nothing
    else
        case Combine.parse program s of
            Ok ( _, _, result ) ->
                Just <| Ok result

            Err ( _, stream, ms ) ->
                Just <|
                    Err ("parse error: " ++ toString ms ++ ", " ++ toString stream)


program : Parser s (List E)
program =
    manyTill expr end


atom : Parser s E
atom =
    choice [ regexP, phrase, term ]


parsers : Parser s E
parsers =
    lazy <|
        \() ->
            choice [ pair, range, group, atom ]


parsers_ : Parser s E
parsers_ =
    notOp
        |> or mustOp
        |> or mustNotOp
        |> andMap parsers


expr : Parser s E
expr =
    lazy <|
        \() ->
            chainl op (ws <| choice [ parsers_, parsers ])



{- XXX: https://github.com/Bogdanp/elm-combine/issues/14 -}


subexpr : Parser s E
subexpr =
    lazy <|
        \() ->
            choice [ range, group, atom ]


ws : Parser s a -> Parser s a
ws e =
    whitespace *> e <* whitespace


quote : Parser s String
quote =
    string "\""


quotes : Parser s a -> Parser s a
quotes e =
    quote *> e <* quote


{-| Wildcards are included as part of Term for the time being.

    fox
    qu?ck bro*

-}
term : Parser s E
term =
    ETerm
        <$> regex "[\\w\\d*?][\\w\\d*?_-]*"
        <*> fuzziness
        <*> boost
        <?> "term"


{-| "fox quick"
-}
phrase : Parser s E
phrase =
    EPhrase
        <$> quotes (regex "[\\ \\w\\d*?_-]+")
        <*> proximity
        <*> boost
        <?> "phrase"


{-| (fox quick)
-}
group : Parser s E
group =
    lazy <|
        \() ->
            EGroup
                <$> parens (many1 expr)
                <*> boost
                <?> "group"


fieldSep : Parser s String
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
field : Parser s E
field =
    EField
        <$> regex "[\\w_][\\w\\d_]*(\\\\.\\*)?"
        <* fieldSep
        <?> "field"


pair : Parser s E
pair =
    lazy <|
        \() ->
            EPair
                <$> (map (,) field <*> subexpr)
                <?> "pair"


{-|

    _missing_:title

-}
missing : Parser s String
missing =
    string "_missing_"


{-|

    _exists_:title

-}
exists : Parser s String
exists =
    string "_exists_"


slash : Parser s String
slash =
    string "/"


slashes : Parser s a -> Parser s a
slashes e =
    slash *> e <* slash


lbrace : Parser s String
lbrace =
    string "{"


rbrace : Parser s String
rbrace =
    string "}"


lbracket : Parser s String
lbracket =
    string "["


rbracket : Parser s String
rbracket =
    string "]"


{-|

    name:/joh?n(ath[oa]n)/

-}
regexP : Parser s E
regexP =
    ERegex
        <$> slashes (regex "[^\\/]+")
        <*> boost
        <?> "regex"


range : Parser s E
range =
    lazy <|
        \() ->
            ERange
                <$> choice
                        [ inclusiveRange
                        , exclusiveRange
                        , lInclusiveRange
                        , rInclusiveRange
                        , sideUnboundedRange
                        ]


{-| Inclusive range

    date:[2012-01-01 TO 2012-12-31]
    count:[1 TO 5]

-}
inclusiveRange : Parser s Range
inclusiveRange =
    lazy <|
        \() ->
            brackets <|
                Inclusive
                    <$> rangeLowerBound
                    <*> rangeUpperBound
                    <?> "[to]"


{-| Exclusive range

    {1 TO 5}

-}
exclusiveRange : Parser s Range
exclusiveRange =
    lazy <|
        \() ->
            braces <|
                Exclusive
                    <$> rangeLowerBound
                    <*> rangeUpperBound
                    <?> "{to}"


{-| Left inclusive range

    [1 TO 5}

-}
lInclusiveRange : Parser s Range
lInclusiveRange =
    lazy <|
        \() ->
            between lbracket rbrace <|
                LInclusive
                    <$> rangeLowerBound
                    <*> rangeUpperBound
                    <?> "[to}"


{-| Right inclusive range

    {1 TO 5]

-}
rInclusiveRange : Parser s Range
rInclusiveRange =
    lazy <|
        \() ->
            between lbrace rbracket <|
                RInclusive
                    <$> rangeLowerBound
                    <*> rangeUpperBound
                    <?> "{to]"


{-| Side unbounded range

    age:>10
    age:>=10
    age:<10
    age:<=10
    age:(>=10 AND <20)
    age:(+>=10 +<20)

-}
sideUnboundedRange : Parser s Range
sideUnboundedRange =
    lazy <|
        \() ->
            SideUnbounded
                <$> rangeOp
                <*> term
                <?> ">=<"


rangeOp : Parser s RangeOp
rangeOp =
    choice [ gteOp, gtOp, lteOp, ltOp ]


gtOp : Parser s RangeOp
gtOp =
    Gt <$ string ">"


gteOp : Parser s RangeOp
gteOp =
    Gte <$ string ">="


ltOp : Parser s RangeOp
ltOp =
    Lt <$ string "<"


lteOp : Parser s RangeOp
lteOp =
    Lte <$ string "<="


rangeLowerBound : Parser s E
rangeLowerBound =
    term <* rangeInf


rangeUpperBound : Parser s E
rangeUpperBound =
    term


rangeInf : Parser s String
rangeInf =
    ws <| string "TO"


orOp : Parser s (E -> E -> E)
orOp =
    EOr
        <$ (string "OR" <|> string "||")
        <?> "or"


andOp : Parser s (E -> E -> E)
andOp =
    EAnd
        <$ (string "AND" <|> string "&&")
        <?> "and"


notOp : Parser s (E -> E)
notOp =
    ENot
        <$ (string "NOT" <|> string "!")
        <?> "not"
        |> ws


mustOp : Parser s (E -> E)
mustOp =
    EMust
        <$ string "+"
        <?> "must"


mustNotOp : Parser s (E -> E)
mustNotOp =
    EMustNot
        <$ string "-"
        <?> "must not"


{-| NOT takes precedence over AND, which takes precedence over OR.

    (quick OR brown) AND fox
    status:(active OR pending) title:(full text search)^2

-}
op : Parser s (E -> E -> E)
op =
    lazy <|
        \() ->
            orOp <|> andOp


{-| Fuzziness with default edit distance of 2

    quikc~ brwn~ foks~
    quikc~1

-}
fuzziness : Parser s Fuzziness
fuzziness =
    maybe <| string "~" *> (int <|> succeed 2)


{-|

    "fox quick"~5

-}
proximity : Parser s Proximity
proximity =
    maybe <| string "~" *> (int <|> succeed 2)


{-|

    quick^2 fox
    "john smith"^2   (foo bar)^4

-}
boost : Parser s Boost
boost =
    maybe <| string "^" *> (float <|> (toFloat <$> int))


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


reserved : List String -> Parser s String
reserved xs =
    regex <| "[" ++ String.join "" (List.map Regex.escape xs) ++ "]+"


notReserved : List String -> Parser s String
notReserved xs =
    regex <| "[^" ++ String.join "" (List.map Regex.escape xs) ++ "]+"
