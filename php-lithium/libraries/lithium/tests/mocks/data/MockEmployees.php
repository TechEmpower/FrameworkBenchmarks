<?php

namespace lithium\tests\mocks\data;

class MockEmployees extends \lithium\data\Model {
	protected $_meta = array(
		'source' => 'employees',
		'connection' => 'lithium_mysql_test'
	);
}

?>