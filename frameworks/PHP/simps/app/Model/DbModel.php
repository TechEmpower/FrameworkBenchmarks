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

use Simps\DB\BaseModel;

class DbModel extends BaseModel
{
    public function fortunes()
    {
        $fortune = [];
        $this->pdo->fortune_test = $this->pdo->fortune_test ?? $this->pdo->prepare('SELECT id, message FROM Fortune');
        $this->pdo->fortune_test->execute();
        $arr = $this->pdo->fortune_test->fetchAll();
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
        $this->pdo->updates_test_select = $this->pdo->updates_test_select ?? $this->pdo->prepare(
                'SELECT id, randomNumber FROM World WHERE id = ?'
            );
        $this->pdo->updates_test_update = $this->pdo->updates_test_update ?? $this->pdo->prepare(
                'UPDATE World SET randomNumber = ? WHERE id = ?'
            );

        while ($query_count--) {
            $id = mt_rand(1, 10000);
            $randomNumber = mt_rand(1, 10000);
            $this->pdo->updates_test_select->execute([$id]);
            $ret = $this->pdo->updates_test_select->fetchAll();

            // Store result in array.
            $world = ['id' => $id, 'randomNumber' => $ret[0]['randomNumber']];
            $world['randomNumber'] = $randomNumber;
            $this->pdo->updates_test_update->execute([$randomNumber, $id]);

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
        // Define query
        $this->pdo->db_test = $this->pdo->db_test ?? $this->pdo->prepare(
                'SELECT id, randomNumber FROM World WHERE id = ?'
            );

        // For each query, store the result set values in the response array
        while ($query_count--) {
            $id = mt_rand(1, 10000);
            $this->pdo->db_test->execute([$id]);
            $data = $this->pdo->db_test->fetchAll();

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

    public function microDb()
    {
        $id = mt_rand(1, 10000);
        $data = $this->get(
            "World",
            [
                'id',
                'randomNumber'
            ],
            [
                "id" => $id
            ]
        );
        return \json_encode($data, JSON_NUMERIC_CHECK);
    }

    public function microQueries(int $queries = 0)
    {
        $query_count = 1;
        if ($queries > 1) {
            $query_count = $queries > 500 ? 500 : $queries;
        }

        $arr = [];

        while ($query_count--) {
            $id = mt_rand(1, 10000);
            $data = $this->get(
                "World",
                [
                    'id',
                    'randomNumber'
                ],
                [
                    "id" => $id
                ]
            );

            // Store result in array.
            $arr[] = $data;
        }

        return \json_encode($arr, JSON_NUMERIC_CHECK);
    }

    public function microUpdates(int $queries = 0)
    {
        $query_count = 1;
        if ($queries > 1) {
            $query_count = $queries > 500 ? 500 : $queries;
        }

        $arr = [];

        while ($query_count--) {
            $id = mt_rand(1, 10000);
            $randomNumber = mt_rand(1, 10000);
            $data = $this->get(
                "World",
                [
                    'id',
                    'randomNumber'
                ],
                [
                    "id" => $id
                ]
            );

            $world = ['id' => $id, 'randomNumber' => $data['randomNumber']];
            $world['randomNumber'] = $randomNumber;

            $this->update(
                "World",
                [
                    'randomNumber' => $randomNumber
                ],
                [
                    "id" => $id
                ]
            );

            $arr[] = $world;
        }

        return \json_encode($arr, JSON_NUMERIC_CHECK);
    }
}
