<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\g11n\catalog;

class MockAdapter extends \lithium\g11n\catalog\Adapter {

	public function merge($data, $item) {
		return $this->_merge($data, $item);
	}
}

?>