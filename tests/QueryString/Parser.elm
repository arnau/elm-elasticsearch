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
        , describe "Phrase Modifier"
            [ test "Proximity" <| \() -> parseProximity
            , test "Fuzziness with default" <| \() -> parseProximityDefault
            , test "Boost" <| \() -> parsePhraseBoost
            , test "Boost" <| \() -> parsePhraseProximityBoost
            ]
        ]


parseTerm : Expectation
parseTerm =
    Expect.equal (parse "fox") (Ok [ (ETerm "fox" Nothing Nothing) ])

parseMultipleTerms : Expectation
parseMultipleTerms =
    Expect.equal
        (parse "fox quick")
        (Ok [ ETerm "fox" Nothing Nothing, ETerm "quick" Nothing Nothing ])


parsePhrase : Expectation
parsePhrase =
    Expect.equal (parse "\"fox quick\"") (Ok [ (EPhrase "fox quick" Nothing Nothing) ])


parseGroup : Expectation
parseGroup =
    Expect.equal
        (parse "(fox quick)")
        (Ok [ (EGroup [ ETerm "fox" Nothing Nothing
                      , ETerm "quick" Nothing Nothing
                      ])
            ])


parseField : Expectation
parseField =
    Expect.equal
        (parse "status:active")
        (Ok [ EPair ( EField "status", ETerm "active" Nothing Nothing ) ])


parseFieldWild : Expectation
parseFieldWild =
    Expect.equal
        (parse "book\\.*:quick")
        (Ok [ EPair ( EField "book\\.*", ETerm "quick" Nothing Nothing ) ])


parseFieldWithPhrase : Expectation
parseFieldWithPhrase =
    Expect.equal
        (parse "author:\"John Smith\"")
        (Ok [ EPair ( EField "author", EPhrase "John Smith" Nothing Nothing) ])


parseFieldWithGroup : Expectation
parseFieldWithGroup =
    Expect.equal
        (parse "title:(quick brown)")
        (Ok [ EPair ( EField "title"
                    , EGroup [ ETerm "quick" Nothing Nothing
                             , ETerm "brown" Nothing Nothing
                             ]
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
                (ETerm "quick" Nothing Nothing)
                (ETerm "brown" Nothing Nothing)
            ])


parseOr : Expectation
parseOr =
    Expect.equal
        (parse "quick OR brown")
        (Ok [ EOr
                (ETerm "quick" Nothing Nothing)
                (ETerm "brown" Nothing Nothing)
            ])


parseAndOr : Expectation
parseAndOr =
    Expect.equal
        (parse "quick AND brown OR else")
        (Ok [ EOr
                (EAnd (ETerm "quick" Nothing Nothing) (ETerm "brown" Nothing Nothing))
                (ETerm "else" Nothing Nothing)
            ])


parseNotTerm : Expectation
parseNotTerm =
    Expect.equal
        (parse "NOT quick")
        (Ok [ ENot
                (ETerm "quick" Nothing Nothing)
            ])


parseNotPhrase : Expectation
parseNotPhrase =
    Expect.equal
        (parse "NOT \"fox quick\"")
        (Ok [ ENot
                (EPhrase "fox quick" Nothing Nothing)
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
                    [ ETerm "quick" Nothing Nothing
                    , ETerm "brown" Nothing Nothing
                    ]
            ])


parseNotField : Expectation
parseNotField =
    Expect.equal
        (parse "NOT status:active")
        (Ok [ ENot <|
                EPair
                    ( EField "status", ETerm "active" Nothing Nothing)
            ])


parseInclusiveRange : Expectation
parseInclusiveRange =
    Expect.equal
        (parse "[1 TO 5]")
        (Ok [ ERange <|
                Inclusive
                    (ETerm "1" Nothing Nothing)
                    (ETerm "5" Nothing Nothing)
            ])

parseInclusiveRangeWithField : Expectation
parseInclusiveRangeWithField =
    Expect.equal
        (parse "date:[2012-01-01 TO 2012-12-31]")
        (Ok [ EPair <|
                ( EField "date"
                , ERange <|
                    Inclusive
                        (ETerm "2012-01-01" Nothing Nothing)
                        (ETerm "2012-12-31" Nothing Nothing)
                )
            ])


parseExclusiveRange : Expectation
parseExclusiveRange =
    Expect.equal
        (parse "{alpha TO omega}")
        (Ok [ ERange <|
                Exclusive
                    (ETerm "alpha" Nothing Nothing)
                    (ETerm "omega" Nothing Nothing)
            ])


parseLInclusiveRange : Expectation
parseLInclusiveRange =
    Expect.equal
        (parse "[alpha TO omega}")
        (Ok [ ERange <|
                LInclusive
                    (ETerm "alpha" Nothing Nothing)
                    (ETerm "omega" Nothing Nothing)
            ])

parseRInclusiveRange : Expectation
parseRInclusiveRange =
    Expect.equal
        (parse "{alpha TO omega]")
        (Ok [ ERange <|
                RInclusive
                    (ETerm "alpha" Nothing Nothing)
                    (ETerm "omega" Nothing Nothing)
            ])


parseGtRange : Expectation
parseGtRange =
    Expect.equal
        (parse ">10")
        (Ok [ ERange <| SideUnbounded Gt (ETerm "10" Nothing Nothing)
            ])


parseGteRange : Expectation
parseGteRange =
    Expect.equal
        (parse ">=10")
        (Ok [ ERange <| SideUnbounded Gte (ETerm "10" Nothing Nothing)
            ])


parseLtRange : Expectation
parseLtRange =
    Expect.equal
        (parse "<10")
        (Ok [ ERange <| SideUnbounded Lt (ETerm "10" Nothing Nothing)
            ])


parseLteRange : Expectation
parseLteRange =
    Expect.equal
        (parse "<=10")
        (Ok [ ERange <| SideUnbounded Lte (ETerm "10" Nothing Nothing)
            ])


parseFuzziness : Expectation
parseFuzziness =
    Expect.equal
        (parse "quick~1")
        (Ok [ ETerm "quick" (Just 1) Nothing
            ])

parseFuzzinessDefault : Expectation
parseFuzzinessDefault =
    Expect.equal
        (parse "quick~")
        (Ok [ ETerm "quick" (Just 2) Nothing
            ])


parseFuzzinessWithField : Expectation
parseFuzzinessWithField =
    Expect.equal
        (parse "is:quick~")
        (Ok [ EPair <|
                ( EField "is", ETerm "quick" (Just 2) Nothing )
            ])


parseTermBoost : Expectation
parseTermBoost =
    Expect.equal
        (parse "quick^2 fox")
        (Ok [ ETerm "quick" Nothing (Just 2)
            , ETerm "fox" Nothing Nothing
            ])


parseTermFuzzinessBoost : Expectation
parseTermFuzzinessBoost =
    Expect.equal
        (parse "quick~1^2")
        (Ok [ ETerm "quick" (Just 1) (Just 2)
            ])


parseProximity : Expectation
parseProximity =
    Expect.equal
        (parse "\"quick fox\"~2")
        (Ok [ EPhrase "quick fox" (Just 2) Nothing
            ])


parseProximityDefault : Expectation
parseProximityDefault =
    Expect.equal
        (parse "\"quick fox\"~")
        (Ok [ EPhrase "quick fox" (Just 2) Nothing
            ])


parsePhraseBoost : Expectation
parsePhraseBoost =
    Expect.equal
        (parse "\"quick fox\"^2")
        (Ok [ EPhrase "quick fox" Nothing (Just 2)
            ])


parsePhraseProximityBoost : Expectation
parsePhraseProximityBoost =
    Expect.equal
        (parse "quick~1^2")
        (Ok [ ETerm "quick" (Just 1) (Just 2)
            ])
