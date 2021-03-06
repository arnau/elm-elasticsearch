# Elm Elasticsearch

**WARNING**: Exploratory phase.


This is a library for interacting with
[Elasticsearch](https://www.elastic.co/products/elasticsearch).  The target
version is [Elasticsearch
2.4](https://www.elastic.co/guide/en/elasticsearch/reference/2.4/index.html).


## Features

* [x] Query string parser (ref: https://www.elastic.co/guide/en/elasticsearch/reference/2.4/query-dsl-query-string-query.html#query-string-syntax)
  * [x] Field names (e.g. `status:active`)
  * [x] Wildcards (e.g. `qu?ck bro*`)
  * [x] Regular expressions (e.g. `/joh?n(ath[oa]n)/`)
  * [x] Fuzziness (e.g. `quikc~ brwn~ foks~`)
  * [x] Proximity searches (e.g. `"fox quick"~5`)
  * [x] Ranges (e.g. `date:[2012-01-01 TO 2012-12-31]`)
  * [x] Boosting (e.g. `quick^2 fox`)
  * [x] Simple Boolean operators (e.g. `quick brown +fox -news`)
  * [x] Boolean operators (e.g. `((quick AND fox) OR (brown AND fox) OR fox) AND NOT news`)
  * [x] Grouping (e.g. `(quick OR brown) AND fox`)
  * [x] Empty query
* [ ] Simple query string parser (ref: https://www.elastic.co/guide/en/elasticsearch/reference/2.4/query-dsl-simple-query-string-query.html#_simple_query_string_syntax)
* [ ] Search API (ref: https://www.elastic.co/guide/en/elasticsearch/reference/2.4/search.html)
  * [x] Count API
  * [x] Search API
  * [ ] Request Body Search
  * [ ] Validate API
  * [ ] Explain API


## License

elm-elasticsearch is distributed under the terms of the MIT License. See
[LICENSE](LICENSE) for details.
