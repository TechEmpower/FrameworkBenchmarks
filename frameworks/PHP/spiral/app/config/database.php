<?php

/**
 * Spiral Framework.
 *
 * @license   MIT
 * @author    Anton Titov (Wolfy-J)
 */
declare(strict_types=1);

use Cycle\Database\Config;

return [
    'default' => 'default',
    'databases' => [
        'default' => ['driver' => 'mysql'],
    ],
    'drivers' => [
        'mysql' => new Config\MySQLDriverConfig(
            connection: new Config\MySQL\DsnConnectionConfig(env('DB_DSN')),
            queryCache: true
        ),
    ],
];
