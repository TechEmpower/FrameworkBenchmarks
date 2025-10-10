<?php

return [
    'basePath' => realpath(__DIR__ . '/../'),
    'defaultLayoutPath' => '@app/modules/site/layouts',
    'defaultLayout' => 'main',
    'language' => 'en',
    'components' => [
        'Piko\View' => [],
        'Piko\Router' => [
            'construct' => [
                [
                    'routes' => require __DIR__ . '/routes.php',
                ]
            ]
        ],
        'PDO' => [
            'construct' => [
                (getenv('DATABASE_DRIVER') ? getenv('DATABASE_DRIVER') : 'mysql') . ':host=tfb-database;dbname=hello_world',
                'benchmarkdbuser',
                'benchmarkdbpass'
            ]
        ],
    ],
    'modules' => [
        'site' => 'app\modules\site\Module'
    ]
];
