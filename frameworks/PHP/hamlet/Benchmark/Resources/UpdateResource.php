<?php

namespace Benchmark\Resources;

use Benchmark\Repositories\WorldRepository;
use Hamlet\Http\Entities\JsonEntity;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Responses\{Response, SimpleOKResponse};

class UpdateResource extends DbResource
{
    use QueriesCountTrait;

    public function getResponse(Request $request): Response
    {
        $repository = new WorldRepository;
        $count = $this->getQueriesCount($request);
        $entries = $this->database->withSessions(array_map(
            fn () => $repository->findById(mt_rand(1, 10000)),
            range(1, $count),
        ));
        $modifiedEntries = array_map(
            fn ($entry) => $entry->withNumber(mt_rand(1, 10000)),
            $entries
        );
        $this->database->withSessions(array_map(
            fn ($modifiedEntry) => $repository->updateNumber($modifiedEntry),
            $modifiedEntries
        ));
        return new SimpleOKResponse(new JsonEntity($modifiedEntries));
    }
}
