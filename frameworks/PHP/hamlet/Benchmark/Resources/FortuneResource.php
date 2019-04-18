<?php

namespace Benchmark\Resources;

use Benchmark\Entities\FortuneEntity;
use Benchmark\Entities\Message;
use Hamlet\Database\Database;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Resources\HttpResource;
use Hamlet\Http\Responses\Response;
use Hamlet\Http\Responses\SimpleOKResponse;

class FortuneResource implements HttpResource
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
        $messages = $procedure->processAll()->selectAll()->cast(Message::class)->collectAll();
        $messages[] = new Message(0, 'Additional fortune added at request time.');
        usort($messages, function (Message $a, Message $b): int {
            return $a->message() <=> $b->message();
        });
        return new SimpleOKResponse(new FortuneEntity($messages));
    }
}
