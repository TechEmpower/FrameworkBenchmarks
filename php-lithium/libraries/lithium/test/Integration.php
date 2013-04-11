<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\test;

/**
 * This is the base class for integration tests.
 *
 * Integration tests are for determining that different parts of the framework will work
 * together (integrate) as expected. An example of a common integration test would be for
 * ensuring that an adapter interacts correctly with the class it is designed to interface
 * with. Example: the `Session` class and the `Php` adapter. Unit tests will ensure that
 * both the `Session` and `Php` classes behave correctly under isolation, while an integration
 * test ensures that the two classes interact and interface correctly.
 */
class Integration extends \lithium\test\Unit {

	/**
	 * Auto init for applying Integration filter to this test class.
	 *
	 * @return void
	 */
	protected function _init() {
		parent::_init();

		$this->applyFilter('run', function($self, $params, $chain) {
			$before = $self->results();

			$chain->next($self, $params, $chain);

			$after = $self->results();

			while (count($after) > count($before)) {
				$result = array_pop($after);
				if ($result['result'] === 'fail') {
					return false;
				}
			}
		});
	}
}

?>