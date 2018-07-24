<?php

namespace Benchmark\Resources;

use Benchmark\Entities\FortuneEntity;
use Hamlet\Database\Database;
use Hamlet\Requests\Request;
use Hamlet\Resources\WebResource;
use Hamlet\Responses\OKResponse;
use Hamlet\Responses\Response;

class FortuneResource implements WebResource
{
    private $database;

    public function __construct(Database $database)
    {
        $this->database = $database;
    }

    public function getResponse(Request $request): Response
    {
        $query = '
            SELECT id,
                   message
              FROM Fortune
        ';
        $procedure = $this->database->prepare($query);
        $messages = $procedure->processAll()
            ->map('id', 'message')->flatten()
            ->collectAll();
        $messages[0] = 'Additional fortune added at request time.';
        asort($messages);
        return new OKResponse(new FortuneEntity($messages));
    }
}
