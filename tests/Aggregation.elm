module Aggregation exposing (all)

import Test exposing (..)
import Expect exposing (Expectation)

import Json.Decode as Decode exposing
    ( (:=)
    , Decoder
    , decodeString
    , float
    , int
    , keyValuePairs
    , list
    , maybe
    , string
    , succeed
    )
import Json.Decode.Extra exposing ((|:))

import Elasticsearch.Aggregation.Decode as AggD exposing (Aggregation(..))


all : Test
all =
    describe "Aggregation"
        [ describe "Decode"
            [ test "Aggregation by name" <| \() -> decodeAggByName
            , test "Avg" <| \() -> decodeAvg
            , test "Avg by name" <| \() -> decodeAvgByName
            , test "Avgs" <| \() -> decodeAvgs
            ]
        ]


decodeAggByName : Expectation
decodeAggByName =
    decodeString (AggD.aggregation "foo") """{"foo": {"value": 1}, "bar": {"value": 2}}"""
        |> Expect.equal (Ok (Avg "foo" 1))


decodeAvg : Expectation
decodeAvg =
    decodeString (AggD.avg "avg_grade") """{"avg_grade": {"value": 75}}"""
        |> Expect.equal (Ok (Avg "avg_grade" 75))


decodeAvgByName : Expectation
decodeAvgByName =
    decodeString (AggD.avg "foo") """{"foo": {"value": 1}, "bar": {"value": 2}}"""
        |> Expect.equal (Ok (Avg "foo" 1))


decodeAvgs : Expectation
decodeAvgs =
    decodeString AggD.avgs """{"foo": {"value": 1}, "bar": {"value": 2}}"""
        |> Expect.equal (Ok [Avg "bar" 2, Avg "foo" 1])
