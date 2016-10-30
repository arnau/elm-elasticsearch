module Elasticsearch.Aggregation.Decode exposing (..)

import Json.Decode as Decode exposing
    ( (:=)
    , Decoder
    , float
    , int
    , keyValuePairs
    , string
    , succeed
    )
import Json.Decode.Extra exposing ((|:))


{-| Aggregation name -}
type alias Name = String

type alias Field = String

type alias Missing a = Maybe a


{-|

TODO: script option

    {
        "avg_grade": {
            "value": 75
        }
    }
-}
type Aggregation
    = Avg Name Float


aggregation : Name -> Decoder Aggregation
aggregation name =
    Decode.oneOf
        [ avg name
        ]


{-| Average decoder expects a specific name to get the right value.
-}
avg : Name -> Decoder Aggregation
avg name =
    succeed (Avg name)
        |: (Decode.at [name, "value"] float)


{-| Multiple average aggregations.  **Unstable**
-}
avgs : Decoder (List Aggregation)
avgs =
    Decode.map toAvg (keyValuePairs ("value" := float))


toAvg : List (Name, Float) -> List Aggregation
toAvg xs =
    List.map (\(k, v) -> Avg k v) xs
