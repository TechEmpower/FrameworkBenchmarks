<?php

namespace Benchmark\Resources;

use Benchmark\Entities\RandomNumber;
use Hamlet\Database\{Database, Session};
use Hamlet\Http\Entities\JsonEntity;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Resources\HttpResource;
use Hamlet\Http\Responses\{Response, SimpleOKResponse};

class DbResource implements HttpResource
{
    /** @var Database */
    protected $database;

    public function __construct(Database $database)
    {
        $this->database = $database;
    }

    public function getResponse(Request $request): Response
    {
        $id = mt_rand(1, 10000);
        $record = $this->database->withSession(
            function (Session $session) use ($id) {
                $procedure = $session->prepare('
                    SELECT id,
                           randomNumber 
                      FROM World 
                     WHERE id = ?
                ');
                $procedure->bindInteger($id);
                return $procedure->processOne()
                    ->selectAll()->cast(RandomNumber::class)
                    ->collectHead();
            }
        );
        return new SimpleOKResponse(new JsonEntity($record));
    }
}
