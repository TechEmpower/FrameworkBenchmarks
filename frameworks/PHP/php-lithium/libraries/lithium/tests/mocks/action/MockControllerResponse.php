<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\action;

class MockControllerResponse extends \lithium\action\Response {

	public $hasRendered = false;

	public function render() {
		$this->hasRendered = true;
	}
}

?>