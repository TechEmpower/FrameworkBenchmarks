#!/bin/bash

curl -XDELETE http://localhost:9200/tfb

curl -XPUT 'http://localhost:9200/tfb' -d '
{
  "settings": {
    "index": {
      "number_of_shards": 1,
      "number_of_replicas": 1
    }
  },
  "mappings": {
    "world": {
      "properties": {
        "randomNumber": { "type" : "integer", "index" : "not_analyzed" }
      }
    }
  }
}
'
