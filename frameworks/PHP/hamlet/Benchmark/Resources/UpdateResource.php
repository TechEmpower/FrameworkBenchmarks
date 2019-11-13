<?php

namespace Benchmark\Resources;

use Benchmark\Entities\RandomNumber;
use Hamlet\Database\{Procedure, Session};
use Hamlet\Http\Entities\JsonEntity;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Resources\HttpResource;
use Hamlet\Http\Responses\{Response, SimpleOKResponse};

class UpdateResource implements HttpResource
{
    use QueriesCountTrait;

    /** @var Procedure */
    private $selectProcedure;

    /** @var Procedure */
    private $updateProcedure;

    public function __construct(Session $session)
    {
        $this->selectProcedure = $session->prepare('
            SELECT id,
                   randomNumber 
              FROM World
             WHERE id = ?
        ');
        $this->updateProcedure = $session->prepare('
            UPDATE World
               SET randomNumber = ? 
             WHERE id = ?
        ');
    }

    public function getResponse(Request $request): Response
    {
        $count = $this->getQueriesCount($request);

        $payload = [];
        while ($count--) {
            $id = mt_rand(1, 10000);
            $randomNumber = mt_rand(1, 10000);

            $this->selectProcedure->bindInteger($id);
            /** @var RandomNumber $entry */
            $entry = $this->selectProcedure->processOne()
                ->selectAll()->cast(RandomNumber::class)
                ->collectHead();
            $modifiedEntry = $entry->withNumber($randomNumber);

            $this->updateProcedure->bindInteger($modifiedEntry->number());
            $this->updateProcedure->bindInteger($modifiedEntry->id());
            $this->updateProcedure->execute();

            $payload[] = $modifiedEntry;
        }

        return new SimpleOKResponse(new JsonEntity($payload));
    }
}
