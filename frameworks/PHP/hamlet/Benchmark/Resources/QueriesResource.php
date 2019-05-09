<?php

namespace Benchmark\Resources;

use Benchmark\Entities\RandomNumber;
use Hamlet\Database\Database;
use Hamlet\Database\Procedure;
use Hamlet\Http\Entities\JsonEntity;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Responses\Response;
use Hamlet\Http\Responses\SimpleOKResponse;

class QueriesResource extends DbResource
{
    /** @var Procedure */
    private $procedure;

    public function __construct(Database $database)
    {
        parent::__construct($database);
        $query = '
            SELECT id,
                   randomNumber 
              FROM World 
             WHERE id = ?
        ';
        $this->procedure = $this->database->prepare($query);
    }

    public function getResponse(Request $request): Response
    {
        $count = $this->getQueriesCount($request);

        $payload = [];
        while ($count-- > 0) {
            $id = mt_rand(1, 10000);
            $this->procedure->bindInteger($id);
            $payload[] = $this->procedure->processOne()
                ->selectAll()->cast(RandomNumber::class)
                ->collectHead();
        }

        return new SimpleOKResponse(new JsonEntity($payload));
    }
}
