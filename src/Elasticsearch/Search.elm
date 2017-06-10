module Elasticsearch.Search
    exposing
        ( Param(..)
        , Search
        , decode
        , fetch
        )

{-| Search API.

Source: <https://www.elastic.co/guide/en/elasticsearch/reference/2.4/search-search.html>

@docs decode, fetch

@docs Search, Param

-}

import Elasticsearch.Shards as Shards exposing (Shards)
import Http
import Json.Decode as Decode
    exposing
        ( Decoder
        , field
        , float
        , int
        , list
        , maybe
        , string
        , succeed
        )
import Json.Decode.Extra exposing ((|:))
import String
import Url


{-| Get the documents for the given search query.

It composes a request to `/{+set}/_search` as defined in <https://www.elastic.co/guide/en/elasticsearch/reference/2.4/search-uri-request.html>

    fetch "http://localhost:9200" [ Count.Query "foo" ] tweetDecode
        |> Task.perform FetchFailure FetchSuccess

TODO: Support the Query API

-}
fetch : String -> List Param -> Decoder a -> Http.Request (Search a)
fetch url_ params source =
    Http.get (Url.url (url_ ++ "/_search") (List.map paramToPair params)) (decode source)


{-| Allowed parameters for the Search endpoint.

Source: <https://www.elastic.co/guide/en/elasticsearch/reference/2.4/search-uri-request.html#_parameters_3>

TODO: Implement decoder influencers: Fields, Explain, Source

-}
type Param
    = Query String
    | AnalizeWildcard Bool
    | Analizer String
    | DefaultField String
    | DefaultOperator String
    | Explain Bool
    | Fields (List String)
    | Lenient Bool
    | LowercaseExpandedTerms Bool
    | Sort String
    | Source Bool
    | TerminateAfter Bool
    | Timeout Bool
    | TrackScores Bool
    | From Int
    | Size Int
    | SearchType SearchType


type SearchType
    = QueryThenFetch
    | DfQueryThenFetch
    | Scan


{-| Response from the Search API.

    { hits = hits
    , shards =
        { total = 5
        , successful = 5
        , failed = 0
        }
    }

-}
type alias Search a =
    { shards : Shards
    , hits : Hits a
    }


{-| Hits subresponse.

    hits =
        { total = 1
        , hits = [ hit ]
        , maxScore = Nothing
        }

-}
type alias Hits a =
    { total : Int
    , hits : List (Hit a)
    , maxScore : Maybe Float
    }


{-| Hit subresponse.

    hit =
        { index = "twitter"
        , type' = "tweet"
        , id = "1"
        , score = Nothing
        , source = tweet
        }

    tweet =
        { user = "kimchy"
        , postDate = "2009-11-15T14:12:12"
        , message = "trying out Elasticsearch"
        }

-}
type alias Hit a =
    { index : String
    , type_ : String
    , id : String
    , score : Maybe Float
    , source : a
    }


{-| Search response decoder.

    type alias Tweet =
        { user : String
        , postDate : String
        , message : String
        }

    tweetDecode : Decoder Tweet
    tweetDecode =
        succeed Tweet
            |: (field "user" string)
            |: (field "postDate" string)
            |: (field "message" string)

    Json.Decode.decodeString
        tweetDecode """{"user" : "kimchy", "postDate" : "2009-11-15T14:12:12", "message" : "trying out Elasticsearch"}"""

    -- { shards =
    --     { total = 5
    --     , successful = 5
    --     , failed = 0
    --     }
    -- , hits =
    --     { total = 1
    --     , hits =
    --         [ { index = "twitter"
    --           , type' = "tweet"
    --           , id = "1"
    --           , score = Nothing
    --           , source =
    --                 { user = "kimchy"
    --                 , postDate = "2009-11-15T14:12:12"
    --                 , message = "trying out Elasticsearch"
    --                 }
    --           }
    --         ]
    --     , maxScore = Nothing
    --     }
    -- }

-}
decode : Decoder a -> Decoder (Search a)
decode source =
    succeed Search
        |: field "_shards" Shards.decode
        |: field "hits" (hits source)


{-| Hits decoder
-}
hits : Decoder a -> Decoder (Hits a)
hits source =
    succeed Hits
        |: field "total" int
        |: field "hits" (list (hit source))
        |: maybe (field "max_score" float)


{-| Hit decoder
-}
hit : Decoder a -> Decoder (Hit a)
hit source =
    succeed Hit
        |: field "_index" string
        |: field "_type" string
        |: field "_id" string
        |: maybe (field "_score" float)
        |: field "_source" source


paramToPair : Param -> ( String, String )
paramToPair param =
    case param of
        Query value ->
            ( "q", value )

        Analizer value ->
            ( "analizer", value )

        AnalizeWildcard value ->
            ( "analize_wildcard", String.toLower (toString value) )

        DefaultField value ->
            ( "df", value )

        DefaultOperator value ->
            ( "default_operator", value )

        Explain value ->
            ( "explain", String.toLower (toString value) )

        Fields value ->
            ( "fields", String.join "," value )

        Lenient value ->
            ( "lenient", String.toLower (toString value) )

        LowercaseExpandedTerms value ->
            ( "lovercase_expanded_terms", String.toLower (toString value) )

        Sort value ->
            ( "sort", value )

        Source value ->
            ( "_source", String.toLower (toString value) )

        TerminateAfter value ->
            ( "terminate_after", String.toLower (toString value) )

        Timeout value ->
            ( "timeout", String.toLower (toString value) )

        TrackScores value ->
            ( "track_scores", String.toLower (toString value) )

        From value ->
            ( "from", toString value )

        Size value ->
            ( "size", toString value )

        SearchType value ->
            ( "search_type", searchTypeToString value )


searchTypeToString value =
    case value of
        QueryThenFetch ->
            "query_then_fetch"

        DfQueryThenFetch ->
            "df_query_then_fetch"

        Scan ->
            "scan"
