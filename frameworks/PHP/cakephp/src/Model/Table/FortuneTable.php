<?php

namespace App\Model\Table;

use App\Model\Entity\Fortune;
use Cake\ORM\Table;

class FortuneTable extends Table
{
    protected ?string $_table = 'fortune';
    protected ?string $_entityClass = Fortune::class;
}
