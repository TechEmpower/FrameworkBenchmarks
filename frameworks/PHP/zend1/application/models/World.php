<?php
class Model_World extends Zend_Db_Table_Abstract {
	protected $_name = 'World';

	public function __construct() {
		parent::__construct(array(
			'rowClass' => 'Model_WorldRow'
		));
	}
}
