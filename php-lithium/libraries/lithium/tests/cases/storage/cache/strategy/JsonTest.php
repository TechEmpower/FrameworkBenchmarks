<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\storage\cache\strategy;

use lithium\storage\cache\strategy\Json;

class JsonTest extends \lithium\test\Unit {

	public function setUp() {
		$this->Json = new Json();
	}

	public function testWrite() {
		$data = array('some' => 'data');
		$result = $this->Json->write($data);
		$expected = json_encode($data);
		$this->assertEqual($expected, $result);
	}

	public function testRead() {
		$expected = array('some' => 'data');
		$encoded = json_encode($expected);
		$result = $this->Json->read($encoded);
		$this->assertEqual($expected, $result);
	}
}

?>