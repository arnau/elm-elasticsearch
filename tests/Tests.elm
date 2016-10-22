module Tests exposing (..)

import Test exposing (..)
import Expect
import String

import QueryString
import QueryString.Parser
import Count

all : Test
all =
    describe "Elasticsearch"
        [ QueryString.all
        , QueryString.Parser.all
        , Count.all
        ]
