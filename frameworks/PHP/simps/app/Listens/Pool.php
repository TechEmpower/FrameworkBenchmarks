<?php

declare(strict_types=1);
/**
 * This file is part of Simps.
 *
 * @link     https://simps.io
 * @document https://doc.simps.io
 * @license  https://github.com/simple-swoole/simps/blob/master/LICENSE
 */

namespace App\Listens;

use Simps\DB\PDO;
use Simps\Singleton;

class Pool
{
    use Singleton;

    public function workerStart($server, $workerId)
    {
        $config = config('database', []);
        if (! empty($config)) {
            PDO::getInstance($config);
        }
    }
}
