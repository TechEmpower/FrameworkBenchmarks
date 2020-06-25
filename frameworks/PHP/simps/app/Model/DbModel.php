<?php

declare(strict_types=1);
/**
 * This file is part of Simps.
 *
 * @link     https://simps.io
 * @document https://doc.simps.io
 * @license  https://github.com/simple-swoole/simps/blob/master/LICENSE
 */

namespace App\Model;

use Simps\DB\DB;

class DbModel extends DB
{
    public function fortunes()
    {
        $fortune = [];
        $arr = $this->query('SELECT id, message FROM Fortune');
        foreach ($arr as $row) {
            $fortune[$row['id']] = $row['message'];
        }
        $fortune[0] = 'Additional fortune added at request time.';
        \asort($fortune);
        return $fortune;
    }

    public function updates(int $queries = 0)
    {
        $query_count = 1;
        if ($queries > 1) {
            $query_count = $queries > 500 ? 500 : $queries;
        }

        $arr = [];

        while ($query_count--) {
            $id = mt_rand(1, 10000);
            $randomNumber = mt_rand(1, 10000);

            $ret = $this->query('SELECT id, randomNumber FROM World WHERE id = ?', [$id]);

            // Store result in array.
            $world = ['id' => $id, 'randomNumber' => $ret[0]['randomNumber']];
            $world['randomNumber'] = $randomNumber;

            $this->execute('UPDATE World SET randomNumber = ? WHERE id = ?', [$randomNumber, $id]);

            $arr[] = $world;
        }

        return \json_encode($arr, JSON_NUMERIC_CHECK);
    }

    public function db(int $queries = 0)
    {
        // Read number of queries to run from URL parameter
        $query_count = 1;
        if ($queries > 1) {
            $query_count = $queries > 500 ? 500 : $queries;
        }

        // Create an array with the response string.
        $arr = [];

        // For each query, store the result set values in the response array
        while ($query_count--) {
            $id = mt_rand(1, 10000);
            $data = $this->query('SELECT id, randomNumber FROM World WHERE id = ?', [$id]);

            // Store result in array.
            $arr[] = ['id' => $id, 'randomNumber' => $data[0]['randomNumber']];
        }

        // Use the PHP standard JSON encoder.
        // http://www.php.net/manual/en/function.json-encode.php
        if ($queries === -1) {
            $arr = $arr[0];
        }

        return \json_encode($arr, JSON_NUMERIC_CHECK);
    }
}
