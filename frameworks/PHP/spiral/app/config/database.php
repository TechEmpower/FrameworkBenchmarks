<?php
/**
 * Spiral Framework.
 *
 * @license   MIT
 * @author    Anton Titov (Wolfy-J)
 */
declare(strict_types=1);

use Spiral\Database\Driver;

return [
    'default'   => 'default',
    'databases' => [
        'default' => ['driver' => 'mysql'],
    ],
    'drivers'   => [
        'mysql' => [
            'driver'     => Driver\MySQL\MySQLDriver::class,
            'connection' => 'mysql:host=tfb-database:3306;charset=utf8;dbname=hello_world',
            'username'   => 'benchmarkdbuser',
            'password'   => 'benchmarkdbpass',
        ],
    ]
];