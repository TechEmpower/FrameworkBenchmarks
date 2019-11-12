<?php

namespace Benchmark\Resources;

use Benchmark\Entities\RandomNumber;
use Hamlet\Database\{Procedure, Session};
use Hamlet\Http\Entities\JsonEntity;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Resources\HttpResource;
use Hamlet\Http\Responses\{Response, SimpleOKResponse};

class DbResource implements HttpResource
{
    /** @var Procedure */
    private $procedure;

    public function __construct(Session $session)
    {
        $this->procedure = $session->prepare('
            SELECT id,
                   randomNumber 
              FROM World 
             WHERE id = ?
        ');
    }

    public function getResponse(Request $request): Response
    {
        $id = mt_rand(1, 10000);
        $this->procedure->bindInteger($id);
        $record = $this->procedure->processOne()
            ->selectAll()->cast(RandomNumber::class)
            ->collectHead();
        return new SimpleOKResponse(new JsonEntity($record));
    }
}
