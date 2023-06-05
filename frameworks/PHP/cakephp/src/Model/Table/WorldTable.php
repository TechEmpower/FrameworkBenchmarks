<?php

namespace App\Model\Table;

use App\Model\Entity\World;
use Cake\ORM\Table;

class WorldTable extends Table
{
    protected $_table = 'world';
    protected $_entityClass = World::class;
}
