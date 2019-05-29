<?php

namespace Benchmark\Resources;

use Benchmark\Entities\RandomNumber;
use Hamlet\Database\Database;
use Hamlet\Database\Procedure;
use Hamlet\Http\Entities\JsonEntity;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Responses\Response;
use Hamlet\Http\Responses\SimpleOKResponse;

class UpdateResource extends DbResource
{
    /** @var Procedure */
    private $selectProcedure;

    /** @var Procedure */
    private $updateProcedure;

    public function __construct(Database $database)
    {
        parent::__construct($database);
        $selectQuery = '
            SELECT id,
                   randomNumber 
              FROM World
             WHERE id = ?
        ';
        $this->selectProcedure = $this->database->prepare($selectQuery);
        $updateQuery = '
            UPDATE World 
               SET randomNumber = ? 
             WHERE id = ?
        ';
        $this->updateProcedure = $this->database->prepare($updateQuery);
    }

    public function getResponse(Request $request): Response
    {
        $count = $this->getQueriesCount($request);

        $payload = [];
        while ($count-- > 0) {
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
