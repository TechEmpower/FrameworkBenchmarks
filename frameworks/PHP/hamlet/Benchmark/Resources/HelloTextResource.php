<?php

namespace Benchmark\Resources;

use Hamlet\Http\Entities\PlainTextEntity;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Resources\HttpResource;
use Hamlet\Http\Responses\{Response, SimpleOKResponse};

class HelloTextResource implements HttpResource
{
    public function getResponse(Request $request): Response
    {
        $entity = new PlainTextEntity('Hello, World!');
        return new SimpleOKResponse($entity);
    }
}
