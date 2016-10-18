<?php
class Model_Fortune extends Zend_Db_Table_Abstract {
	protected $_name = 'Fortune';

	public function __construct() {
		parent::__construct(array(
			'rowClass' => 'Model_FortuneRow'
		));
	}
}
