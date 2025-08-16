<?php

declare(strict_types=1);

return [
    'generator' => [
        'amqp' => [
            'consumer' => [
                'namespace' => 'App\Amqp\Consumers',
            ],
            'producer' => [
                'namespace' => 'App\Amqp\Producers',
            ],
        ],
        'aspect' => [
            'namespace' => 'App\Aspects',
        ],
        'command' => [
            'namespace' => 'App\Console\Commands',
        ],
        'controller' => [
            'namespace' => 'App\Http\Controllers',
        ],
        'job' => [
            'namespace' => 'App\Jobs',
        ],
        'listener' => [
            'namespace' => 'App\Listeners',
        ],
        'middleware' => [
            'namespace' => 'App\Http\Middleware',
        ],
        'process' => [
            'namespace' => 'App\Processes',
        ],
        'request' => [
            'namespace' => 'App\Http\Requests',
        ],
        'model' => [
            'namespace' => 'App\Models',
            'uses' => App\Models\Model::class,
        ],
        'factory' => [
            'path' => 'database/factories',
        ],
        'seeder' => [
            'path' => 'database/seeders',
        ],
        'event' => [
            'namespace' => 'App\Events',
        ],
        'provider' => [
            'namespace' => 'App\Providers',
        ],
        'component' => [
            'namespace' => 'App\View\Component',
        ],
        'channel' => [
            'namespace' => 'App\Broadcasting',
            'uses' => App\Models\User::class,
        ],
        'observer' => [
            'namespace' => 'App\Observers',
            'model_namespace' => 'App\Models',
        ],
        'mail' => [
            'namespace' => 'App\Mail',
        ],
        'notification' => [
            'namespace' => 'App\Notifications',
        ],
        'policy' => [
            'namespace' => 'App\Policies',
            'model_namespace' => 'App\Models',
        ],
        'rule' => [
            'namespace' => 'App\Rules',
        ],
        'resource' => [
            'namespace' => 'App\Http\Resources',
        ],
    ],
];
