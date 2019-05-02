<?php

namespace Benchmark\Resources;

use Benchmark\Entities\RandomNumber;
use function Hamlet\Cast\_int;
use Hamlet\Database\Database;
use Hamlet\Http\Entities\JsonEntity;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Resources\HttpResource;
use Hamlet\Http\Responses\Response;
use Hamlet\Http\Responses\SimpleOKResponse;

class QueriesResource implements HttpResource
{
    protected $database;

    public function __construct(Database $database)
    {
        $this->database = $database;
    }

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

    protected function getQueriesCount(Request $request): int
    {
        if ($request->hasQueryParam('queries')) {
            $count = $request->getQueryParam('queries', _int());
            if ($count < 1) {
                return 1;
            } elseif (500 < $count) {
                return 500;
            } else {
                return $count;
            }
        } else {
            return 1;
        }
    }
}
