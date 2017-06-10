module Search exposing (suite)

import Elasticsearch.Search as Search
import Expect exposing (Expectation)
import Json.Decode as Decode
    exposing
        ( Decoder
        , decodeString
        , field
        , float
        , int
        , list
        , maybe
        , string
        , succeed
        )
import Json.Decode.Extra exposing ((|:))
import Test exposing (..)


suite : Test
suite =
    describe "Search suite"
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
        |: field "user" string
        |: field "postDate" string
        |: field "message" string


tweet =
    { user = "kimchy"
    , postDate = "2009-11-15T14:12:12"
    , message = "trying out Elasticsearch"
    }


hit =
    { index = "twitter"
    , type_ = "tweet"
    , id = "1"
    , score = Nothing
    , source = tweet
    }


hits =
    { total = 1
    , hits = [ hit ]
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
        res =
            decodeString (Search.decode tweetDecoder) rawTweets
    in
    case res of
        Err e ->
            Expect.fail e

        Ok x ->
            List.map .source x.hits.hits
                |> Expect.equal [ tweet ]
