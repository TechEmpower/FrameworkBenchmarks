<?php

declare(strict_types=1);

use Hypervel\Cache\SwooleStore;
use Hypervel\Support\Str;

return [
    /*
    |--------------------------------------------------------------------------
    | Default Cache Store
    |--------------------------------------------------------------------------
    |
    | This option controls the default cache connection that gets used while
    | using this caching library. This connection is used when another is
    | not explicitly specified when executing a given caching function.
    |
    */

    'default' => env('CACHE_DRIVER', 'array'),

    /*
    |--------------------------------------------------------------------------
    | Cache Stores
    |--------------------------------------------------------------------------
    |
    | Here you may define all of the cache "stores" for your application as
    | well as their drivers. You may even define multiple stores for the
    | same cache driver to group types of items stored in your caches.
    |
    | Supported drivers: "array", "file", "redis", "swoole", "stack", "null"
    |
    */

    'stores' => [
        'array' => [
            'driver' => 'array',
            'serialize' => false,
        ],

        'file' => [
            'driver' => 'file',
            'path' => storage_path('cache/data'),
            'lock_path' => storage_path('cache/data'),
        ],

        'redis' => [
            'driver' => 'redis',
            'connection' => 'default',
            'lock_connection' => 'default',
        ],

        'swoole' => [
            'driver' => 'swoole',
            'table' => 'default',
            'memory_limit_buffer' => 0.05,
            'eviction_policy' => SwooleStore::EVICTION_POLICY_LRU,
            'eviction_proportion' => 0.05,
            'eviction_interval' => 10000, // milliseconds
        ],

        'stack' => [
            'driver' => 'stack',
            'stores' => [
                'swoole' => [
                    'ttl' => 3, // seconds
                ],
                'redis',
            ],
        ],

        'database' => [
            'driver' => 'database',
            'connection' => env('DB_CACHE_CONNECTION', env('DB_CONNECTION', 'default')),
            'table' => env('DB_CACHE_TABLE', 'cache'),
            'lock_connection' => env('DB_CACHE_LOCK_CONNECTION'),
            'lock_table' => env('DB_CACHE_LOCK_TABLE', 'cache_locks'),
            'lock_lottery' => [2, 100],
            'lock_timeout' => 86400,
        ],
    ],

    'swoole_tables' => [
        'default' => [
            'rows' => 1024,
            'bytes' => 10240,
            'conflict_proportion' => 0.2,
        ],
    ],

    /*
    |--------------------------------------------------------------------------
    | Cache Key Prefix
    |--------------------------------------------------------------------------
    |
    | When utilizing a RAM based store such as APC or Memcached, there might
    | be other applications utilizing the same cache. So, we'll specify a
    | value to get prefixed to all our keys so we can avoid collisions.
    |
    */

    'prefix' => env('CACHE_PREFIX', Str::slug(env('APP_NAME', 'hypervel'), '_') . '_cache'),
];
