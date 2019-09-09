<?php

return [
    'base_path' => env('APP_BASE_PATH', APP_BASE_PATH),
    'switch' => envInt('STORAGE_SWITCH', 0),
    'storage_path' => env('STORAGE_PATH', __DIR__ . '/../runtime/storage/'),
    'types' => envArray('STORAGE_TYPES', ['file', 'qiniu', 'alioss']),
    'ext' => [
        'qiniu' => [
            'default_bucket' => env('QINIU_DEFAULT_BUCKET', 'default'),
            'buckets' => [
                env('QINIU_DEFAULT_BUCKET', 'default') => [
                    'access_key' => env('QINIU_DEFAULT_ACCESS_KEY', ''),
                    'secret_key' => env('QINIU_DEFAULT_SECRET_KEY', ''),
                    'domain' => env('QINIU_DEFAULT_DOMAIN', ''),
                ],
            ],
        ],
        'alioss' => [
            'default_bucket' => env('ALIOSS_DEFAULT_BUCKET', 'default'),
            'buckets' => [
                env('ALIOSS_DEFAULT_BUCKET', 'default') => [
                    'access_id' => env('ALIOSS_DEFAULT_ACCESS_ID', ''),
                    'access_secret' => env('ALIOSS_DEFAULT_ACCESS_SECRET', ''),
                    'endpoint' => env('ALIOSS_DEFAULT_ENDPOINT', ''),
                    'timeout' => envDouble('ALIOSS_DEFAULT_TIMEOUT', 1),
                    'connectTimeout' => envDouble('ALIOSS_DEFAULT_CONNECT_TIMEOUT', 1),
                    'isCName' => envBool('ALIOSS_DEFAULT_IS_CNAME', false),
                    'securityToken' => env('ALIOSS_DEFAULT_SECURITY_TOKEN', null),
                    'domain' => env('ALIOSS_DEFAULT_DOMAIN', ''),
                ],
            ],
        ],
    ],
];
