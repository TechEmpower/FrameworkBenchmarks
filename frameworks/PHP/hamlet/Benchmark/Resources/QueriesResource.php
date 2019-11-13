<?php

namespace Benchmark\Resources;

use Benchmark\Entities\RandomNumber;
use Hamlet\Database\Session;
use Hamlet\Http\Entities\JsonEntity;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Resources\HttpResource;
use Hamlet\Http\Responses\{Response, SimpleOKResponse};

class QueriesResource extends DbResource
{
    use QueriesCountTrait;

    public function getResponse(Request $request): Response
    {
        $count = $this->getQueriesCount($request);
        $callables = [];
        while ($count--) {
            $callables[] = function (Session $session) {
                $id = mt_rand(1, 10000);
                $procedure = $session->prepare('
                    SELECT id,
                           randomNumber 
                      FROM World 
                     WHERE id = ?
                ');
                $procedure->bindInteger($id);
                return $procedure->processOne()
                    ->selectAll()->cast(RandomNumber::class)
                    ->collectHead();
            };
        }
        $payload = $this->database->withSessions($callables);
        return new SimpleOKResponse(new JsonEntity($payload));
    }
}
