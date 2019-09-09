<?php

return [
    'connections' => [
        'default' => [
            'hosts' => [
                env('ES_DEFAULT_HOST', '127.0.0.1:9200'),
            ],
            'timeout' => envDouble('ES_TIMEOUT', 1),
        ],
    ],
    'switch' => envInt('ES_SWITCH', 0),
];
