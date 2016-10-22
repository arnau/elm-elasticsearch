module Count exposing (all)

import Test exposing (..)
import Expect exposing (Expectation)

import Json.Decode exposing (decodeString)

import Elasticsearch.Count as Count exposing (Count)


all : Test
all =
    describe "Count"
        [ test "Decode count" <| \() -> decodeCount
        ]

count : Count
count =
    { count = 1
    , shards =
        { total = 5
        , successful = 5
        , failed = 0
        }
    }


raw : String
raw =
    """{"count": 1, "_shards": { "total": 5, "successful": 5, "failed": 0 }}"""


decodeCount : Expectation
decodeCount =
    decodeString Count.decode raw
        |> Expect.equal (Ok count)
