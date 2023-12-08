<?php

namespace App\Model\Table;

use App\Model\Entity\World;
use Cake\ORM\Table;

class WorldTable extends Table
{
    protected ?string $_table = 'world';
    protected ?string $_entityClass = World::class;
}
