<?php
namespace Model{
	class Tree extends \PHPixie\ORM\Model
	{

		public $has_one = array('fairy');
		public $connection = 'orm';
		public $belongs_to = array(
			'protector' => array('model' => 'fairy', 'key' => 'protector_id')
		);

	}
}