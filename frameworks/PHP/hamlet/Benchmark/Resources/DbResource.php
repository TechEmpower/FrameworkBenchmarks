<?php

namespace Benchmark\Resources;

use Benchmark\Entities\RandomNumber;
use Hamlet\Database\Database;
use Hamlet\Http\Entities\JsonEntity;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Resources\HttpResource;
use Hamlet\Http\Responses\Response;
use Hamlet\Http\Responses\SimpleOKResponse;

class DbResource implements HttpResource
{
    private $database;

    public function __construct(Database $database)
    {
        $this->database = $database;
    }

    public function getResponse(Request $request): Response
    {
        $id = mt_rand(1, 10000);
        $query = '
            SELECT id,
                   randomNumber 
              FROM World 
             WHERE id = ?
        ';
        $procedure = $this->database->prepare($query);
        $procedure->bindInteger($id);
        $record = $procedure->processOne()->selectAll()->cast(RandomNumber::class)->collectHead();

        return new SimpleOKResponse(new JsonEntity($record));
    }
}
