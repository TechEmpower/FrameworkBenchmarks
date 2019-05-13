<?php

namespace Benchmark\Resources;

use Benchmark\Entities\FortuneEntity;
use Benchmark\Entities\Message;
use Hamlet\Database\Database;
use Hamlet\Database\Procedure;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Responses\Response;
use Hamlet\Http\Responses\SimpleOKResponse;

class FortuneResource extends DbResource
{
    /** @var Procedure */
    private $procedure;

    public function __construct(Database $database)
    {
        parent::__construct($database);
        $query = '
            SELECT id,
                   message
              FROM Fortune
        ';
        $this->procedure = $this->database->prepare($query);
    }

    public function getResponse(Request $request): Response
    {
        $messages = $this->procedure->processAll()
            ->selectAll()->cast(Message::class)
            ->collectAll();

        $messages[] = new Message(0, 'Additional fortune added at request time.');
        usort($messages, function (Message $a, Message $b): int {
            return $a->message() <=> $b->message();
        });
        return new SimpleOKResponse(new FortuneEntity($messages));
    }
}
