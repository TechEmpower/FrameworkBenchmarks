<?php
class Model_FortuneRow extends Zend_Db_Table_Row_Abstract {
	public function __construct($config = array()) {
		if ( array_key_exists('data', $config) ) {
			$config['data']['id'] = (int) $config['data']['id'];
			$config['data']['message'] = (int) $config['data']['message'];
		}
		parent::__construct($config);
	}
}
