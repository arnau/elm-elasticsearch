module Search exposing (all)

import Test exposing (..)
import Expect exposing (Expectation)

import Json.Decode as Decode exposing
    ( (:=)
    , Decoder
    , int
    , float
    , list
    , maybe
    , string
    , succeed
    , decodeString
    )
import Json.Decode.Extra exposing ((|:))
import Elasticsearch.Search as Search


all : Test
all =
    describe "Search"
        [ test "Decode source" <| \() -> decodeTweet
        , test "Decode search" <| \() -> decodeSearch
        , test "Extract results" <| \() -> extractResults
        ]


-- Fixtures


type alias Tweet =
    { user : String
    , postDate : String
    , message : String
    }

tweetDecoder : Decoder Tweet
tweetDecoder =
    succeed Tweet
        |: ("user" := string)
        |: ("postDate" := string)
        |: ("message" := string)

tweet =
    { user = "kimchy"
    , postDate = "2009-11-15T14:12:12"
    , message = "trying out Elasticsearch"
    }

hit =
    { index = "twitter"
    , type' = "tweet"
    , id = "1"
    , score = Nothing
    , source = tweet
    }

hits =
    { total = 1
    , hits = [hit]
    , maxScore = Nothing
    }

search =
    { shards =
        { total = 5
        , successful = 5
        , failed = 0
        }
    , hits = hits
    }


rawTweets : String
rawTweets =
    """
    {
        "_shards":{
            "total" : 5,
            "successful" : 5,
            "failed" : 0
        },
        "hits":{
            "total" : 1,
            "hits" : [
                {
                    "_index" : "twitter",
                    "_type" : "tweet",
                    "_id" : "1",
                    "_source" : {
                        "user" : "kimchy",
                        "postDate" : "2009-11-15T14:12:12",
                        "message" : "trying out Elasticsearch"
                    }
                }
            ]
        }
    }
    """


-- Tests


decodeTweet : Expectation
decodeTweet =
    decodeString tweetDecoder """{"user" : "kimchy", "postDate" : "2009-11-15T14:12:12", "message" : "trying out Elasticsearch"}"""
        |> Expect.equal (Ok tweet)


decodeSearch : Expectation
decodeSearch =
    decodeString (Search.decode tweetDecoder) rawTweets
        |> Expect.equal (Ok search)


extractResults : Expectation
extractResults =
    let
        res = decodeString (Search.decode tweetDecoder) rawTweets
    in
        case res of
            Err e ->
                Expect.fail e

            Ok x ->
                List.map .source x.hits.hits
                    |> Expect.equal [tweet]
