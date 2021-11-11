<?php

declare(strict_types=1);

return [
    'simpleWorkerStart' => [
        [\App\Listens\Pool::class, 'workerStart'],
    ],
];