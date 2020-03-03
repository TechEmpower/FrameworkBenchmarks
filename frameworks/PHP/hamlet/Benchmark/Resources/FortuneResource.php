<?php

namespace Benchmark\Resources;

use Benchmark\Entities\FortuneEntity;
use Benchmark\Entities\Message;
use Hamlet\Database\Session;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Resources\HttpResource;
use Hamlet\Http\Responses\{Response, SimpleOKResponse};

class FortuneResource extends DbResource
{
    public function getResponse(Request $request): Response
    {
        $messages = $this->database->withSession(
            function (Session $session) {
                $procedure = $session->prepare('
                    SELECT id,
                           message
                      FROM Fortune
                ');
                return $procedure->processAll()
                    ->selectAll()->cast(Message::class)
                    ->collectAll();
            }
        );
        $messages[] = new Message(0, 'Additional fortune added at request time.');
        usort($messages, function (Message $a, Message $b): int {
            return $a->message() <=> $b->message();
        });
        return new SimpleOKResponse(new FortuneEntity($messages));
    }
}
