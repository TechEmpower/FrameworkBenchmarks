<?php

namespace Benchmark\Resources;

use Benchmark\Entities\RandomNumber;
use Hamlet\Http\Entities\JsonEntity;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Responses\Response;
use Hamlet\Http\Responses\SimpleOKResponse;

class QueriesResource extends DbResource
{
    public function getResponse(Request $request): Response
    {
        $count = $this->getQueriesCount($request);

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
