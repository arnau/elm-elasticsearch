module Elasticsearch.Shards exposing (Shards, decode)

import Json.Decode as Decode exposing
    ( (:=)
    , Decoder
    , int
    , succeed
    )
import Json.Decode.Extra exposing ((|:))


type alias Shards =
    { total : Int
    , successful : Int
    , failed : Int
    }

decode : Decoder Shards
decode =
    succeed Shards
        |: ("total" := int)
        |: ("successful" := int)
        |: ("failed" := int)
