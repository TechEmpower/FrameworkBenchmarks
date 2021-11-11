<?php

namespace App\Model\Entity;

use Cake\Datasource\EntityInterface;
use Cake\ORM\Entity;

class World extends Entity
{
	protected $_accessible = [
		'*' => true,
	];
}