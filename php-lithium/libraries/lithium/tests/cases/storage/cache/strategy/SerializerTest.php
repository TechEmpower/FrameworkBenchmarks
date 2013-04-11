<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\storage\cache\strategy;

use lithium\storage\cache\strategy\Serializer;

class SerializerTest extends \lithium\test\Unit {

	public function setUp() {
		$this->Serializer = new Serializer();
	}

	public function testWrite() {
		$data = array('some' => 'data');
		$result = $this->Serializer->write($data);
		$expected = serialize($data);
		$this->assertEqual($expected, $result);
	}

	public function testRead() {
		$encoded = 'a:1:{s:4:"some";s:4:"data";}';
		$expected = unserialize($encoded);
		$result = $this->Serializer->read($encoded);
		$this->assertEqual($expected, $result);
	}
}

?>