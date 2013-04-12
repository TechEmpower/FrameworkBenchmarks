<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\data;

use lithium\data\Schema;

class SchemaTest extends \lithium\test\Unit {

	public function testShortHandTypeDefinitions() {
		$schema = new Schema(array('fields' => array(
			'id' => 'int',
			'name' => 'string',
			'active' => array('type' => 'boolean', 'default' => true)
		)));

		$this->assertEqual('int', $schema->type('id'));
		$this->assertEqual('string', $schema->type('name'));
		$this->assertEqual('boolean', $schema->type('active'));
		$this->assertEqual(array('type' => 'int'), $schema->fields('id'));
		$this->assertEqual(array('id', 'name', 'active'), $schema->names());

		$expected = array(
			'id' => array('type' => 'int'),
			'name' => array('type' => 'string'),
			'active' => array('type' => 'boolean', 'default' => true)
		);
		$this->assertEqual($expected, $schema->fields());
	}
}

?>