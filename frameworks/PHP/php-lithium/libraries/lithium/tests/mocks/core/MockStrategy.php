<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\core;

class MockStrategy extends \lithium\core\Adaptable {

	protected static $_configurations = array();

	protected static $_strategies = 'strategy.storage.cache';
}

?>