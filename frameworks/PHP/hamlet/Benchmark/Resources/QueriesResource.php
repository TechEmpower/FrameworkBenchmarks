<?php

namespace Benchmark\Resources;

use Benchmark\Entities\RandomNumber;
use Hamlet\Database\Database;
use Hamlet\Http\Entities\JsonEntity;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Resources\HttpResource;
use Hamlet\Http\Responses\Response;
use Hamlet\Http\Responses\SimpleOKResponse;

class QueriesResource implements HttpResource
{
    private $database;

    public function __construct(Database $database)
    {
        $this->database = $database;
    }

    public function getResponse(Request $request): Response
    {
        $count = $request->parameter('queries');
        if ($count === null || $count < 1) {
            $count = 1;
        } else {
            $count = min($count, 500);
        }

        $query = '
            SELECT id,
                   randomNumber 
              FROM World 
             WHERE id = ?
        ';
        $procedure = $this->database->prepare($query);

        $payload = [];
        while ($count-- > 0) {
            $id = mt_rand(1, 10000);
            $procedure->bindInteger($id);
            $payload[] = $procedure->processOne()->selectAll()->cast(RandomNumber::class)->collectHead();
        }

        return new SimpleOKResponse(new JsonEntity($payload));
    }
}
