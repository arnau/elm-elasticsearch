run:
	docker run -d \
             --name es_test \
             -p 9292:9200 \
             -v $(PWD)/es.config:/usr/share/elasticsearch/config/elasticsearch.yml \
             elasticsearch

rm:
	docker rm -vf es_test

load:
	curl -XPOST 'localhost:9292/twitter/tweet/1' \
       -d '{"user" : "kimchy", "postDate" : "2009-11-15T14:12:12", "message" : "trying out Elasticsearch"}'
