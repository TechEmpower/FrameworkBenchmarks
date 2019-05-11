<?php

namespace Benchmark\Resources;

use Benchmark\Entities\RandomNumber;
use Hamlet\Database\Database;
use Hamlet\Database\Procedure;
use Hamlet\Http\Entities\JsonEntity;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Resources\HttpResource;
use Hamlet\Http\Responses\Response;
use Hamlet\Http\Responses\SimpleOKResponse;
use function Hamlet\Cast\_int;

class DbResource implements HttpResource
{
    /** @var Database */
    protected $database;

    /** @var Procedure */
    private $procedure;

    public function __construct(Database $database)
    {
        $this->database = $database;
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
        $id = mt_rand(1, 10000);
        $this->procedure->bindInteger($id);
        $record = $this->procedure->processOne()
            ->selectAll()->cast(RandomNumber::class)
            ->collectHead();
        return new SimpleOKResponse(new JsonEntity($record));
    }

    protected function getQueriesCount(Request $request): int
    {
        if ($request->hasQueryParam('queries')) {
            $count = $request->getQueryParam('queries', _int());
            if ($count < 1) {
                return 1;
            } elseif (500 < $count) {
                return 500;
            } else {
                return $count;
            }
        } else {
            return 1;
        }
    }
}
