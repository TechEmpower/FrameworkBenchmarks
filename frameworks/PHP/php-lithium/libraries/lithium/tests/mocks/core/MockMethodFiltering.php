<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\core;

class MockMethodFiltering extends \lithium\core\Object {

	public function method($data) {
		$data[] = 'Starting outer method call';
		$result = $this->_filter(__METHOD__, compact('data'), function($self, $params, $chain) {
			$params['data'][] = 'Inside method implementation';
			return $params['data'];
		});
		$result[] = 'Ending outer method call';
		return $result;
	}

	public function method2() {
		$filters =& $this->_methodFilters;
		$method = function($self, $params, $chain) use (&$filters) {
			return $filters;
		};
		return $this->_filter(__METHOD__, array(), $method);
	}

	public function manual($filters) {
		$method = function($self, $params, $chain) {
			return "Working";
		};
		return $this->_filter(__METHOD__, array(), $method, $filters);
	}

}

?>