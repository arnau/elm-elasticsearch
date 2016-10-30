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
            , test "Aggs" <| \() -> decodeAggs
            ]
        ]


decodeAggByName : Expectation
decodeAggByName =
    decodeString (AggD.aggregation "foo") """{"foo": {"value": 1}, "bar": {"value": 2}}"""
        |> Expect.equal (Ok (Basic "foo" 1))


decodeAggs : Expectation
decodeAggs =
    decodeString AggD.aggs """{"foo": {"value": 1}, "bar": {"value": 2}}"""
        |> Expect.equal (Ok [Basic "bar" 2, Basic "foo" 1])
