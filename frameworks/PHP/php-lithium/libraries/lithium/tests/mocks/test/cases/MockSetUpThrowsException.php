<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\test\cases;

use Exception;

class MockSetUpThrowsException extends \lithium\test\Unit {

	public function setUp() {
		throw new Exception('setUp throws exception');
	}

	public function testNothing() {
		$this->assert(true);
	}

}

?>