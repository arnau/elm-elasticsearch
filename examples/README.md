# Examples

## Parser showcase

Lists all implemented variants according to the Elasticsearch documentation.

To run it:

```sh
elm reactor
open http://localhost:8000/Parser.elm
```


## Counter

You need to run an instance of Elasticsearch on `localhost:9292` with CORS
configured to allow connections from `localhost:8000`.  If you have
Docker installed you can use `make run` to get Elasticsearch on 9292.

Then run the example:

```sh
elm reactor
open http://localhost:8000/Counter.elm
```


## Search

You need to run an instance of Elasticsearch on `localhost:9292` with CORS
configured to allow connections from `localhost:8000`.  If you have
Docker installed you can use `make run` to get Elasticsearch on 9292.

Then run the example:

```sh
make load # adds one tweet
elm reactor
open http://localhost:8000/Search.elm
```
