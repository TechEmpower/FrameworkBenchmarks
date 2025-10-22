<?php

namespace App\Model\Entity;

use Cake\ORM\Entity;

class World extends Entity
{
    protected array $_accessible = [
        '*' => true,
    ];
}
