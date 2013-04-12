<?php

namespace lithium\tests\mocks\data;

class MockCompanies extends \lithium\data\Model {
	public $hasMany = array(
		'Employees' => array(
			'key' => array('id' => 'company_id'),
			'to' => 'lithium\\tests\\mocks\\data\\MockEmployees'
		)
	);
	protected $_meta = array(
		'source' => 'companies',
		'connection' => 'lithium_mysql_test'
	);
}

?>