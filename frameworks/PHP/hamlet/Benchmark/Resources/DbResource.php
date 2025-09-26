<?php

namespace Benchmark\Resources;

use Benchmark\Repositories\WorldRepository;
use Hamlet\Database\{Database};
use Hamlet\Http\Entities\JsonEntity;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Resources\HttpResource;
use Hamlet\Http\Responses\{Response, SimpleOKResponse};

class DbResource implements HttpResource
{
    public function __construct(protected Database $database) {}

    public function getResponse(Request $request): Response
    {
        $repository = new WorldRepository;
        $id = mt_rand(1, 10000);
        $record = $this->database->withSession($repository->findById($id));
        return new SimpleOKResponse(new JsonEntity($record));
    }
}
