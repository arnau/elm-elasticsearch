module Elasticsearch.Aggregation exposing (..)


{-|

Source: https://www.elastic.co/guide/en/elasticsearch/reference/5.0/search-aggregations.html

    "aggregations" : {
        "<aggregation_name>" : {
            "<aggregation_type>" : {
                <aggregation_body>
            }
            [,"meta" : {  [<meta_data_body>] } ]?
            [,"aggregations" : { [<sub_aggregation>]+ } ]?
        }
        [,"<aggregation_name_2>" : { ... } ]*
    }

-}
type alias Aggregations = List Aggregation


{-| Aggregation name -}
type alias Name = String

type alias Field = String

type alias Missing a = Maybe a

type Aggregation
    {-
        Req:

        {
            "aggs" : {
                "avg_grade" : { "avg" : { "field" : "grade" } }
            }
        }

        Res:

            {
                ...

                "aggregations": {
                    "avg_grade": {
                        "value": 75
                    }
                }
            }
    -}
    = Avg Name Field (Missing Float) -- Metrics type


type alias Meta =
    String
