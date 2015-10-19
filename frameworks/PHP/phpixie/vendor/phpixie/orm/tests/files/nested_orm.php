<?php
namespace Model {
	class Nested extends \PHPixie\ORM\Model
	{
		public $table = 'nested';
		public $connection = 'orm';
		protected $extensions = array(
			'nested' => '\PHPixie\ORM\Extension\Nested'
		);
	}
}