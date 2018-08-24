<?php

namespace Benchmark\Resources;

use Hamlet\Database\Database;
use Hamlet\Entities\JsonEntity;
use Hamlet\Requests\Request;
use Hamlet\Resources\WebResource;
use Hamlet\Responses\OKResponse;
use Hamlet\Responses\Response;

class DbResource implements WebResource
{
    private $database;

    public function __construct(Database $database)
    {
        $this->database = $database;
    }

    public function getResponse(Request $request): Response
    {
        $queryParams = $request->getQueryParams();
        $count = $queryParams['queries'] ?? null;
        if ($count !== null && $count > 0) {
            $count = min($count, 500);
        } else {
            $count = 1;
        }

        $query = '
            SELECT id,
                   randomNumber 
              FROM World 
             WHERE id = ?
        ';
        $procedure = $this->database->prepare($query);

        $payload = [];
        while ($count-- > 0) {
            $id = mt_rand(1, 10000);
            $procedure->bindInteger($id);
            $payload[] = $procedure->fetchOne();
        }

        if (!isset($queryParams['queries'])) {
            $payload = $payload[0];
        }

        return new OKResponse(new JsonEntity($payload));
    }
}
