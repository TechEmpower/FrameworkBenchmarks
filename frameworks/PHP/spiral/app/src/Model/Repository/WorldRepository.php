<?php

declare(strict_types=1);

namespace App\Model\Repository;

use App\Model\World;
use Cycle\ORM\Select\Repository;

class WorldRepository extends Repository
{
    /**
     * @throws \Exception
     */
    public function findRandom(): ?World
    {
        return $this->findByPK(random_int(1, 10000));
    }
}
