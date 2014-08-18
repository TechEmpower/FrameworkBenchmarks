<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD(http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\data;

class MockPostObject {

	public $id;

	public $data;

	public function __construct($values) {
		foreach ($values as $key => $value) {
			$this->$key = $value;
		}
	}
}

?>