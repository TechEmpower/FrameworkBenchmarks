<?php

return [
    'host' => env('ELASTICSEARCH_HOST' , '127.0.0.1'),
    'port' => env('ELASTICSEARCH_PORT' , 9200),
    'username' => env('ELASTICSEARCH_USERNAME'),
    'password' => env('ELASTICSEARCH_PASSWORD')
];