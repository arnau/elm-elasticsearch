module Elasticsearch.Count
    exposing
        ( Count
        , Param(..)
        , decode
        , fetch
        )

{-| Count API.

Source: <https://www.elastic.co/guide/en/elasticsearch/reference/2.4/search-count.html>

@docs decode, fetch

@docs Count, Param

-}

import Elasticsearch.Shards as Shards exposing (Shards)
import Http
import Json.Decode as Decode
    exposing
        ( Decoder
        , field
        , int
        , succeed
        )
import Json.Decode.Extra exposing ((|:))
import String
import Url


{-| Get the amount of documents for the given set.

It composes a request to `/{+set}/_count` as defined in <https://www.elastic.co/guide/en/elasticsearch/reference/2.4/search-count.html>

    fetch "http://localhost:9200" [ Count.Query "foo" ]
        |> Task.perform FetchFailure FetchSuccess

TODO: Support the Query API

-}
fetch : String -> List Param -> Http.Request Count
fetch url params =
    Http.get (Url.url (url ++ "/_count") (List.map paramToPair params)) decode


{-| Allowed parameters for the Count endpoint.

Source: <https://www.elastic.co/guide/en/elasticsearch/reference/2.4/search-count.html#_request_parameters>

-}
type Param
    = Query String
    | DefaultField String
    | Analizer String
    | DefaultOperator String
    | Lenient Bool
    | LowercaseExpandedTerms Bool
    | AnalizeWildcard Bool
    | TerminateAfter Bool


{-| Response from the Count API.

    { count = 1
    , shards =
        { total = 5
        , successful = 5
        , failed = 0
        }
    }

-}
type alias Count =
    { count : Int
    , shards : Shards
    }


{-| Count response decoder.

    Json.Decode.decodeString
        decode """{"count": 1, "_shards": { "total": 5, "successful": 5, "failed": 0 }}"""
    -- { count = 1
    -- , shards =
    --     { total = 5
    --     , successful = 5
    --     , failed = 0
    --     }
    -- }

-}
decode : Decoder Count
decode =
    succeed Count
        |: field "count" int
        |: field "_shards" Shards.decode


paramToPair : Param -> ( String, String )
paramToPair param =
    case param of
        Query value ->
            ( "q", value )

        DefaultField value ->
            ( "df", value )

        Analizer value ->
            ( "analizer", value )

        DefaultOperator value ->
            ( "default_operator", value )

        Lenient value ->
            ( "lenient", String.toLower (toString value) )

        LowercaseExpandedTerms value ->
            ( "lovercase_expanded_terms", String.toLower (toString value) )

        AnalizeWildcard value ->
            ( "analize_wildcard", String.toLower (toString value) )

        TerminateAfter value ->
            ( "terminate_after", String.toLower (toString value) )
