<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD,http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\data\source\database\adapter;

class MockPostgreSql extends \lithium\data\source\database\adapter\PostgreSql {

	public function get($var) {
		return $this->{$var};
	}

	protected function _execute($sql) {
		if (preg_match('/DESCRIBE/', $sql)) {
			return $this->_describe;
		}
		return $sql;
	}

	protected $_describe = array(
		array(
		  'Field' => 'id',
		  'Type' => 'integer',
		  'Null' => 'NO',
		  'Key' => 'PRI',
		  'Default' => null,
		  'Extra' => 'auto_increment'
		),
		array(
		  'Field' => 'name',
		  'Type' => 'varchar(1000)',
		  'Null' => 'NO',
		  'Key' => '',
		  'Default' => null,
		  'Extra' => ''
		),
		array(
		  'Field' => 'priority',
		  'Type' => 'int(10)',
		  'Null' => 'NO',
		  'Key' => '',
		  'Default' => '-1',
		  'Extra' => ''
		),
		array(
		  'Field' => 'parent_id',
		  'Type' => 'int(10)',
		  'Null' => 'NO',
		  'Key' => '',
		  'Default' => '0',
		  'Extra' => ''
		),
		array(
		  'Field' => 'active',
		  'Type' => 'tinyint(1)',
		  'Null' => 'NO',
		  'Key' => '',
		  'Default' => '1',
		  'Extra' => ''
		),
		array(
		  'Field' => 'created',
		  'Type' => 'datetime',
		  'Null' => 'YES',
		  'Key' => '',
		  'Default' => null,
		  'Extra' => ''
		),
		array(
		  'Field' => 'updated',
		  'Type' => 'datetime',
		  'Null' => 'YES',
		  'Key' => '',
		  'Default' => null,
		  'Extra' => ''
		),
		array(
		  'Field' => 'type',
		  'Type' => 'char(20)',
		  'Null' => 'NO',
		  'Key' => '',
		  'Default' => 'post',
		  'Extra' => ''
		),
		array(
		  'Field' => 'max',
		  'Type' => 'bigint(20)',
		  'Null' => 'NO',
		  'Key' => '',
		  'Default' => '0',
		  'Extra' => ''
		),
		array(
		  'Field' => 'timestamp',
		  'Type' => 'timestamp',
		  'Null' => 'NO',
		  'Key' => '',
		  'Default' => 'CURRENT_TIMESTAMP',
		  'Extra' => 'on update CURRENT_TIMESTAMP'
		)
	);
}

?>