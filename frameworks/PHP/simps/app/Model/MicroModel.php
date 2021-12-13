<?php
/**
 * User: lufei
 * Date: 2020/6/24
 * Email: lufei@swoole.com
 */

namespace App\Model;

use Simps\DB\BaseModel;

class MicroModel extends BaseModel
{
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