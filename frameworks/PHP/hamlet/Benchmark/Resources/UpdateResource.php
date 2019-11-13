<?php

namespace Benchmark\Resources;

use Benchmark\Entities\RandomNumber;
use Hamlet\Database\Session;
use Hamlet\Http\Entities\JsonEntity;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Resources\HttpResource;
use Hamlet\Http\Responses\{Response, SimpleOKResponse};

class UpdateResource extends DbResource
{
    use QueriesCountTrait;

    public function getResponse(Request $request): Response
    {
        $count = $this->getQueriesCount($request);
        $callables = [];
        while ($count--) {
            $callables[] = function (Session $session) {
                $id = mt_rand(1, 10000);
                $randomNumber = mt_rand(1, 10000);

                $selectProcedure = $session->prepare('
                    SELECT id,
                           randomNumber 
                      FROM World
                     WHERE id = ?
                ');
                $selectProcedure->bindInteger($id);
                /** @var RandomNumber $entry */
                $entry = $selectProcedure->processOne()
                    ->selectAll()->cast(RandomNumber::class)
                    ->collectHead();
                $modifiedEntry = $entry->withNumber($randomNumber);

                $updateProcedure = $session->prepare('
                    UPDATE World
                       SET randomNumber = ? 
                     WHERE id = ?
                ');
                $updateProcedure->bindInteger($modifiedEntry->number());
                $updateProcedure->bindInteger($modifiedEntry->id());
                $updateProcedure->execute();

                return $modifiedEntry;
            };
        }
        $payload = $this->database->withSessions($callables);
        return new SimpleOKResponse(new JsonEntity($payload));
    }
}
