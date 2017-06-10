module Count exposing (suite)

import Elasticsearch.Count as Count exposing (Count)
import Expect exposing (Expectation)
import Json.Decode exposing (decodeString)
import Test exposing (..)


suite : Test
suite =
    describe "Count suite"
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
