<?php

namespace Benchmark\Repositories;

use Benchmark\Entities\RandomNumber;
use Hamlet\Database\Session;

class WorldRepository
{
    /**
     * @return callable(Session):?RandomNumber
     */
    public function findById(int $id): callable
    {
        return fn (Session $session) =>
            $session->prepare('SELECT id, randomNumber FROM World WHERE id = ?')
                ->bindInteger($id)
                ->processOne()
                ->selectAll()->cast(RandomNumber::class)
                ->collectHead();
    }

    /**
     * @return callable(Session):void
     */
    public function updateNumber(RandomNumber $number): callable
    {
        return fn (Session $session) =>
            $session->prepare('UPDATE World SET randomNumber = ? WHERE id = ?')
                ->bindInteger($number->number())
                ->bindInteger($number->id())
                ->execute();
    }
}
