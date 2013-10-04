<?php
namespace Model {
	class Fairy extends \PHPixie\ORM\Model
	{

		public $belongs_to = array('tree');
		public $connection = 'orm';
		public $has_many = array(
			'protects' => array('model' => 'tree', 'key' => 'protector_id'),
			'friends' => array('model' => 'fairy', 'through' => 'friends', 'key' => 'fairy_id', 'foreign_key' => 'friend_id')
		);
		protected $extensions = array(
			'extension' => '\Extension\Test'
		);
		
		public function get($column)
		{
			if ($column == 'test')
				return 5;
		}

	}
}