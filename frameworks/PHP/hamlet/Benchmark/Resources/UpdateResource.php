<?php

namespace Benchmark\Resources;

use Hamlet\Database\Database;
use Hamlet\Entities\JsonEntity;
use Hamlet\Requests\Request;
use Hamlet\Resources\WebResource;
use Hamlet\Responses\OKResponse;
use Hamlet\Responses\Response;

class UpdateResource implements WebResource
{
    private $database;

    public function __construct(Database $database)
    {
        $this->database = $database;
    }

    public function getResponse(Request $request): Response
    {
        $count = $request->getQueryParams()['queries'] ?? null;
        if ($count !== null && $count > 0) {
            $count = min($count, 500);
        } else {
            $count = 1;
        }

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
            $entry = $selectProcedure->fetchOne();
            $entry['randomNumber'] = $randomNumber;

            $updateProcedure->bindInteger($randomNumber);
            $updateProcedure->bindInteger($id);
            $updateProcedure->execute();

            $payload[] = $entry;
        }

        return new OKResponse(new JsonEntity($payload));
    }
}
