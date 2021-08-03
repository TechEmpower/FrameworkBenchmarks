<?php

namespace Benchmark\Repositories;

use Benchmark\Entities\Message;
use Hamlet\Database\Session;

class FortuneRepository
{
    /**
     * @return callable(Session):array<Message>
     */
    public function findAll(): callable
    {
        return fn (Session $session) =>
            $session->prepare('SELECT id, message FROM Fortune')
                ->processAll()
                ->selectAll()->cast(Message::class)
                ->collectAll();
    }
}
