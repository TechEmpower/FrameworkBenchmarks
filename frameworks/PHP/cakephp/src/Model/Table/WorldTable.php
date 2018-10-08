<?php
namespace App\Model\Table;

use Cake\ORM\Query;
use Cake\ORM\Table;

class WorldTable extends Table
{
    public function initialize(array $config)
    {
        $this->setTable('world');
    }

    public function findRandomId(Query $query, array $options)
    {
        return $this->find()
            ->where(['id' => mt_rand(1, 10000)])
            ->enableHydration(false)
            ->limit(1);
    }
}
