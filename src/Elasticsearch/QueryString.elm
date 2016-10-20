module Elasticsearch.QueryString exposing
    (BoolOp, Param, Query, QueryString, encode)

{-| An Elasticsearch query string encoder.

@docs encode

@docs BoolOp, Param, Query, QueryString

-}

import Json.Encode as Encode


{-|
-}
type alias QueryString =
    { query : Query
    , params : List Param
    }


{-|
-}
type BoolOp
    = AND
    | OR


{-|
-}
type alias Query = String


{-| Allowed top level parameters for the `query_string` payload.

Source: https://www.elastic.co/guide/en/elasticsearch/reference/2.4/query-dsl-query-string-query.html#query-dsl-query-string-query

    QueryString.DefaultOperator QueryString.AND -- { "default_operator": "AND" }

-}
type Param
    = Fields (List String)
    | DefaultField String -- _all
    | DefaultOperator BoolOp -- OR
    | Analyzer String
    | AllowLeadingWildcard Bool -- True
    | LowercaseExpandedTerms Bool -- True
    | EnablePositionIncrements Bool -- True
    | FuzzyMaxExpansions Int -- 50
    -- TODO: | Fuzziness  AUTO | 0 | 1 | 2
    | FuzzyPrefixLength Int -- 0
    | PhraseSlop Int -- 0
    | Boost Float -- 1.0
    | AnalyzeWildcard Bool -- False
    | AutoGeneratePhraseQueries Bool -- False
    | MaxDeterminizedStates Int -- 10000
    -- TODO: | MinimumShouldMatch Int | -Int | %
    | Lenient Bool -- False
    | Locale String -- ROOT
    | TimeZone String


{-| Encodes a `QueryString` into JSON for Elasticsearch consumption.

Source: https://www.elastic.co/guide/en/elasticsearch/reference/2.4/query-dsl-query-string-query.html#query-string-syntax


    encode { query = "(quick AND brown) fox"
           , params = [ DefaultField "content" ]
           }

    -- {
    --     "query_string": {
    --         "query": "(quick AND brown) fox",
    --         "default_field": "content"
    --     }
    -- }

-}
encode : QueryString -> Encode.Value
encode { query, params } =
    Encode.object
        [ ( "query_string"
          , Encode.object (encodeQuery query :: (List.map encodeParam params))
          )
        ]


encodeQuery : Query -> ( String, Encode.Value )
encodeQuery value =
    ( "query", Encode.string value )


encodeParam : Param -> ( String, Encode.Value )
encodeParam param =
    case param of
        Fields value ->
            ( "fields", Encode.list (List.map Encode.string value) )
        -- TODO: use_dis_max.  MultiField has to be modeled differently

        DefaultField value ->
            ( "default_field", Encode.string value )

        DefaultOperator value ->
            ( "default_operator", Encode.string (toString value) )

        Analyzer value ->
            ( "analizer", Encode.string value )

        AllowLeadingWildcard value ->
            ( "allow_leading_wildcard", Encode.bool value )

        LowercaseExpandedTerms value ->
            ( "lowercase_expanded_terms", Encode.bool value )

        EnablePositionIncrements value ->
            ( "enable_position_increments", Encode.bool value )

        FuzzyMaxExpansions value ->
            ( "fuzzy_max_expansions", Encode.int value )

        FuzzyPrefixLength value ->
            ( "fuzzy_prefix_length", Encode.int value )

        PhraseSlop value ->
            ( "phrase_slop", Encode.int value )

        Boost value ->
            ( "boost", Encode.float value )

        AnalyzeWildcard value ->
            ( "analyze_wildcard", Encode.bool value )

        AutoGeneratePhraseQueries value ->
            ( "auto_generate_phrase_queries", Encode.bool value )

        MaxDeterminizedStates value ->
            ( "max_determinized_states", Encode.int value )

        Lenient value ->
            ( "lenient", Encode.bool value )

        Locale value ->
            ( "locale", Encode.string value )

        TimeZone value ->
            ( "time_zone", Encode.string value )
