module QueryString.Parser exposing (all)

import Test exposing (..)
import Expect exposing (Expectation)

import Elasticsearch.QueryString.Parser as QS exposing
    (E(..), Range(..), RangeOp(..), parse)


all : Test
all =
    describe "QueryString Parser"
        [ test "Term" <| \() -> parseTerm
        , test "Multiple terms" <| \() -> parseMultipleTerms
        , test "Phrase" <| \() -> parsePhrase
        , test "Group" <| \() -> parseGroup
        , test "Field" <| \() -> parseField
        , test "Wild field" <| \() -> parseFieldWild
        , test "Field with phrase" <| \() -> parseFieldWithPhrase
        , test "Field with group" <| \() -> parseFieldWithGroup
        , test "Regex" <| \() -> parseRegex
        , test "And" <| \() -> parseAnd
        , test "Or" <| \() -> parseOr
        , test "AndOr" <| \() -> parseAndOr
        , test "Not term" <| \() -> parseNotTerm
        , test "Not field" <| \() -> parseNotField
        , test "Not phrase" <| \() -> parseNotPhrase
        , test "Not regex" <| \() -> parseNotRegex
        , test "Not group" <| \() -> parseNotGroup
        , describe "Range"
            [ test "Inclusive" <| \() -> parseInclusiveRange
            , test "Inclusive with field" <| \() -> parseInclusiveRangeWithField
            , test "Exclusive" <| \() -> parseExclusiveRange
            , test "Left inclusive" <| \() -> parseLInclusiveRange
            , test "Right inclusive" <| \() -> parseRInclusiveRange
            , describe "Side unbounded"
                [ test "gt" <| \() -> parseGtRange
                , test "gte" <| \() -> parseGteRange
                , test "lt" <| \() -> parseLtRange
                , test "lte" <| \() -> parseLteRange
                ]
            ]
        ]


parseTerm : Expectation
parseTerm =
    Expect.equal (parse "fox") (Ok [ (ETerm "fox") ])

parseMultipleTerms : Expectation
parseMultipleTerms =
    Expect.equal (parse "fox quick") (Ok [ ETerm "fox", ETerm "quick" ])


parsePhrase : Expectation
parsePhrase =
    Expect.equal (parse "\"fox quick\"") (Ok [ (EPhrase "fox quick") ])


parseGroup : Expectation
parseGroup =
    Expect.equal
        (parse "(fox quick)")
        (Ok [ (EGroup [ ETerm "fox", ETerm "quick" ]) ])


parseField : Expectation
parseField =
    Expect.equal
        (parse "status:active")
        (Ok [ EPair ( EField "status", ETerm "active" ) ])


parseFieldWild : Expectation
parseFieldWild =
    Expect.equal
        (parse "book\\.*:quick")
        (Ok [ EPair ( EField "book\\.*", ETerm "quick" ) ])


parseFieldWithPhrase : Expectation
parseFieldWithPhrase =
    Expect.equal
        (parse "author:\"John Smith\"")
        (Ok [ EPair ( EField "author", EPhrase "John Smith" ) ])


parseFieldWithGroup : Expectation
parseFieldWithGroup =
    Expect.equal
        (parse "title:(quick brown)")
        (Ok [ EPair ( EField "title"
                        , EGroup [ ETerm "quick", ETerm "brown" ]
                        )
            ])


parseRegex : Expectation
parseRegex =
    Expect.equal (parse "/joh?n(ath[oa]n)/") (Ok [ ERegex "joh?n(ath[oa]n)" ])


parseAnd : Expectation
parseAnd =
    Expect.equal
        (parse "quick AND brown")
        (Ok [ EAnd
                (ETerm "quick")
                (ETerm "brown")
            ])


parseOr : Expectation
parseOr =
    Expect.equal
        (parse "quick OR brown")
        (Ok [ EOr
                (ETerm "quick")
                (ETerm "brown")
            ])


parseAndOr : Expectation
parseAndOr =
    Expect.equal
        (parse "quick AND brown OR else")
        (Ok [ EOr
                (EAnd (ETerm "quick") (ETerm "brown"))
                (ETerm "else")
            ])


parseNotTerm : Expectation
parseNotTerm =
    Expect.equal
        (parse "NOT quick")
        (Ok [ ENot
                (ETerm "quick")
            ])


parseNotPhrase : Expectation
parseNotPhrase =
    Expect.equal
        (parse "NOT \"fox quick\"")
        (Ok [ ENot
                (EPhrase "fox quick")
            ])


parseNotRegex : Expectation
parseNotRegex =
    Expect.equal
        (parse "NOT /foo?/")
        (Ok [ ENot
                (ERegex "foo?")
            ])


parseNotGroup : Expectation
parseNotGroup =
    Expect.equal
        (parse "NOT (quick brown)")
        (Ok [ ENot <|
                EGroup
                    [ ETerm "quick"
                    , ETerm "brown"
                    ]
            ])


parseNotField : Expectation
parseNotField =
    Expect.equal
        (parse "NOT status:active")
        (Ok [ ENot <|
                EPair
                    ( EField "status", ETerm "active" )
            ])


parseInclusiveRange : Expectation
parseInclusiveRange =
    Expect.equal
        (parse "[1 TO 5]")
        (Ok [ ERange <|
                Inclusive (ETerm "1") (ETerm "5")
            ])

parseInclusiveRangeWithField : Expectation
parseInclusiveRangeWithField =
    Expect.equal
        (parse "date:[2012-01-01 TO 2012-12-31]")
        (Ok [ EPair <|
                ( EField "date"
                , ERange <|
                    Inclusive (ETerm "2012-01-01") (ETerm "2012-12-31")
                )
            ])


parseExclusiveRange : Expectation
parseExclusiveRange =
    Expect.equal
        (parse "{alpha TO omega}")
        (Ok [ ERange <|
                Exclusive (ETerm "alpha") (ETerm "omega")
            ])


parseLInclusiveRange : Expectation
parseLInclusiveRange =
    Expect.equal
        (parse "[alpha TO omega}")
        (Ok [ ERange <|
                LInclusive (ETerm "alpha") (ETerm "omega")
            ])

parseRInclusiveRange : Expectation
parseRInclusiveRange =
    Expect.equal
        (parse "{alpha TO omega]")
        (Ok [ ERange <|
                RInclusive (ETerm "alpha") (ETerm "omega")
            ])


parseGtRange : Expectation
parseGtRange =
    Expect.equal
        (parse ">10")
        (Ok [ ERange <| SideUnbounded Gt (ETerm "10")
            ])


parseGteRange : Expectation
parseGteRange =
    Expect.equal
        (parse ">=10")
        (Ok [ ERange <| SideUnbounded Gte (ETerm "10")
            ])


parseLtRange : Expectation
parseLtRange =
    Expect.equal
        (parse "<10")
        (Ok [ ERange <| SideUnbounded Lt (ETerm "10")
            ])


parseLteRange : Expectation
parseLteRange =
    Expect.equal
        (parse "<=10")
        (Ok [ ERange <| SideUnbounded Lte (ETerm "10")
            ])
