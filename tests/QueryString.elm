module QueryString exposing (all)

import Test exposing (..)
import Expect exposing (Expectation)

import Json.Encode as Encode
import Elasticsearch.QueryString as QS exposing (QueryString)


all : Test
all =
    describe "QueryString"
        [ test "Encode simple query" <| \() -> encodeSimpleQuery
        , test "Encode query with default field" <| \() -> encodeQueryWithDF
        , test "Encode query with multi fields" <| \() -> encodeQueryWithMF
        , test "Encode query with default operator" <| \() -> encodeQueryWithDOp
        ]


encodeSimpleQuery : Expectation
encodeSimpleQuery =
    Encode.encode 0 (QS.encode (QueryString "foo AND bar" []))
        |> Expect.equal ("""{"query_string":{"query":"foo AND bar"}}""")


encodeQueryWithDF : Expectation
encodeQueryWithDF =
    let
        query = QueryString "foo" [ QS.DefaultField "content" ]
        expected = """{"query_string":{"query":"foo","default_field":"content"}}"""
    in
        Encode.encode 0 (QS.encode query)
            |> Expect.equal expected


encodeQueryWithMF : Expectation
encodeQueryWithMF =
    let
        query = QueryString "foo" [ QS.Fields [ "content", "notes" ] ]
        expected = """{"query_string":{"query":"foo","fields":["content","notes"]}}"""
    in
        Encode.encode 0 (QS.encode query)
            |> Expect.equal expected


encodeQueryWithDOp : Expectation
encodeQueryWithDOp =
    let
        query = QueryString "foo" [ QS.DefaultOperator QS.AND, QS.Boost 1.2 ]
        expected = """{"query_string":{"query":"foo","default_operator":"AND","boost":1.2}}"""
    in
        Encode.encode 0 (QS.encode query)
            |> Expect.equal expected
