<?php

namespace Benchmark\Resources;

use Hamlet\Entities\JsonEntity;
use Hamlet\Entities\PlainTextEntity;
use Hamlet\Requests\Request;
use Hamlet\Resources\WebResource;
use Hamlet\Responses\Response;
use Hamlet\Responses\SimpleOKResponse;

class HelloResource implements WebResource
{
    private $json;

    public function __construct(bool $json)
    {
        $this->json = $json;
    }

    public function getResponse(Request $request): Response
    {
        if ($this->json) {
            $entity = new JsonEntity(['message' => 'Hello, World!']);
        } else {
            $entity = new PlainTextEntity('Hello, World!');
        }
        return new SimpleOKResponse($entity);
    }
}
