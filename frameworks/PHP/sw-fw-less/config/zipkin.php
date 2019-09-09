<?php

return [
    'service_name' => env('ZIPKIN_SERVICE_NAME', 'Sw-Fw-Less'),
    'endpoint_url' => env('ZIPKIN_ENDPOINT_URL', 'http://localhost:9411/api/v2/spans'),
    'sample_rate' => envInt('ZIPKIN_SAMPLE_RATE', 0),
    'body_size' => envInt('ZIPKIN_BODY_SIZE', 5000),
    'curl_timeout' => envInt('ZIPKIN_CURL_TIMEOUT', 1),
    'redis_options' => [
        'queue_name' => env('ZIPKIN_REDIS_QUEUE_NAME', 'queue:zipkin:span'),
        'connection' => env('ZIPKIN_REDIS_CONNECTION', 'zipkin'),
    ],
    'report_type' => env('ZIPKIN_REPORT_TYPE', 'http'),
];
