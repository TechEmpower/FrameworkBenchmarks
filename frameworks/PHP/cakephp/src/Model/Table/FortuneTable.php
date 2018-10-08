<?php
namespace App\Model\Table;

use Cake\ORM\Table;

class FortuneTable extends Table
{
    public function initialize(array $config)
    {
        $this->setTable('fortune');
    }
}
