<?php

namespace Benchmark\Resources;

use Benchmark\Entities\FortuneEntity;
use Benchmark\Entities\Message;
use Benchmark\Repositories\FortuneRepository;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Responses\{Response, SimpleOKResponse};

class FortuneResource extends DbResource
{
    public function getResponse(Request $request): Response
    {
        $repository = new FortuneRepository;
        $messages = $this->database->withSession($repository->findAll());
        $messages[] = new Message(0, 'Additional fortune added at request time.');
        usort($messages, function (Message $a, Message $b): int {
            return $a->message() <=> $b->message();
        });
        return new SimpleOKResponse(new FortuneEntity($messages));
    }
}
