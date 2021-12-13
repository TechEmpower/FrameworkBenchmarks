<?php

namespace Benchmark\Resources;

use Benchmark\Repositories\WorldRepository;
use Hamlet\Http\Entities\JsonEntity;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Responses\{Response, SimpleOKResponse};

class QueriesResource extends DbResource
{
    use QueriesCountTrait;

    public function getResponse(Request $request): Response
    {
        $repository = new WorldRepository;
        $count = $this->getQueriesCount($request);
        $payload = $this->database->withSessions(array_map(
            fn () => $repository->findById(mt_rand(1, 10000)),
            range(1, $count)
        ));
        return new SimpleOKResponse(new JsonEntity($payload));
    }
}
