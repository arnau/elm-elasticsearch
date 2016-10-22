run:
	docker run -d \
             --name es_test \
             -p 9292:9200 \
             -v $(PWD)/es.config:/usr/share/elasticsearch/config/elasticsearch.yml \
             elasticsearch

rm:
	docker rm -vf es_test

load:
	curl -XPOST 'localhost:9292/customer/external/1?pretty' \
       -d '{"doc": { "name": "Jane Doe" }}'
