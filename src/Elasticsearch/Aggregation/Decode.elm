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
import Dict exposing (Dict)


{-| Aggregation name -}
type alias Name = String

type alias Field = String

type alias Missing a = Maybe a


{-| Aggregation types

TODO: consider moving it to Aggregation.elm

-}
type Aggregation
    = Basic Name Float -- Avg, Cardinality, Max, Min
    | Extended Name ExtendedStats
    | Geo Name Bounds
    | GeoCentroid Name Field Coord
    | Percentile Name PercentileStats
    | Stats Name BasicStats


type alias BasicStats =
    { count : Int
    , min : Float
    , max : Float
    , avg : Float
    , sum : Float
    }


type alias ExtendedStats =
    { count : Int
    , min : Float
    , max : Float
    , avg : Float
    , sum : Float
    , sumOfSquares : Float
    , variance : Float
    , stdDeviation : Float
    , stdDeviationBounds :
        { upper : Float
        , lower : Float
        }
    }


type alias PercentileStats =
    Dict String Float


type alias Bounds =
    { topLeft : Coord
    , bottomRight : Coord
    }


type alias Coord =
    { lat : Float
    , lon : Float
    }


aggregation : Name -> Decoder Aggregation
aggregation name =
    Decode.oneOf
        [ basic name
        ]


{-| Average decoder expects a specific name to get the right value.
-}
basic : Name -> Decoder Aggregation
basic name =
    succeed (Basic name)
        |: (Decode.at [name, "value"] float)


{-| Multiple average aggregations.  **Unstable**
-}
aggs : Decoder (List Aggregation)
aggs =
    Decode.map toBasic (keyValuePairs ("value" := float))


toBasic : List (Name, Float) -> List Aggregation
toBasic xs =
    List.map (\(k, v) -> Basic k v) xs
