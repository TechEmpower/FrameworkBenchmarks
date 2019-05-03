<?php

namespace Benchmark\Resources;

use Benchmark\Entities\RandomNumber;
use Hamlet\Http\Entities\JsonEntity;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Responses\Response;
use Hamlet\Http\Responses\SimpleOKResponse;

class UpdateResource extends DbResource
{
    public function getResponse(Request $request): Response
    {
        $count = $this->getQueriesCount($request);

        $selectQuery = '
            SELECT id,
                   randomNumber 
              FROM World
             WHERE id = ?
        ';
        $selectProcedure = $this->database->prepare($selectQuery);

        $updateQuery = '
            UPDATE World 
               SET randomNumber = ? 
             WHERE id = ?
        ';
        $updateProcedure = $this->database->prepare($updateQuery);

        $payload = [];
        while ($count-- > 0) {
            $id = mt_rand(1, 10000);
            $randomNumber = mt_rand(1, 10000);

            $selectProcedure->bindInteger($id);
            /** @var RandomNumber $entry */
            $entry = $selectProcedure->processOne()->selectAll()->cast(RandomNumber::class)->collectHead();
            $modifiedEntry = $entry->withNumber($randomNumber);

            $updateProcedure->bindInteger($modifiedEntry->number());
            $updateProcedure->bindInteger($modifiedEntry->id());
            $updateProcedure->execute();

            $payload[] = $modifiedEntry;
        }

        return new SimpleOKResponse(new JsonEntity($payload));
    }
}
