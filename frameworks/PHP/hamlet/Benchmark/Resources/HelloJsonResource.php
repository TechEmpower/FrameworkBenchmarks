<?php

namespace Benchmark\Resources;

use Hamlet\Http\Entities\JsonEntity;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Resources\HttpResource;
use Hamlet\Http\Responses\{Response, SimpleOKResponse};

class HelloJsonResource implements HttpResource
{
    public function getResponse(Request $request): Response
    {
        $entity = new JsonEntity(['message' => 'Hello, World!']);
        return new SimpleOKResponse($entity);
    }
}
