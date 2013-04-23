<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\util\collection;

use lithium\util\collection\Filters;

class FiltersTest extends \lithium\test\Unit {

	public function testRun() {
		$options = array('method' => __FUNCTION__, 'class' => __CLASS__, 'data' => array(
			function($self, $params, $chain) {
				$params['message'] .= 'is a filter chain ';
				return $chain->next($self, $params, $chain);
			},
			function($self, $params, $chain) {
				$params['message'] .= 'in the ' . $chain->method() . ' method ';
				return $chain->next($self, $params, $chain);
			},
			function($self, $params, $chain) {
				return $params['message'] . 'of the ' . $self . ' class.';
			}
		));
		$result = Filters::run(__CLASS__, array('message' => 'This '), $options);
		$expected = 'This is a filter chain in the testRun method of the';
		$expected .= ' lithium\tests\cases\util\collection\FiltersTest class.';
		$this->assertEqual($expected, $result);
	}

	public function testRunWithoutChain() {
		$options = array('method' => __FUNCTION__, 'class' => __CLASS__, 'data' => array(
			function($self, $params, $chain) {
				return $chain->next($self, $params, null);
			},
			'This is a filter chain that calls $chain->next() without the $chain argument.'
		));
		$result = Filters::run(__CLASS__, array(), $options);
		$expected = 'This is a filter chain that calls $chain->next() without the $chain argument.';
		$this->assertEqual($expected, $result);
	}

	public function testLazyApply() {
		$class = 'lithium\tests\mocks\util\MockFilters';

		Filters::apply($class, 'filteredMethod', function($self, $params, $chain) {
			return md5($chain->next($self, $params, $chain));
		});

		$expected = md5('Working?');
		$result = $class::filteredMethod();
		$this->assertEqual($expected, $result);

		Filters::apply($class, 'filteredMethod', function($self, $params, $chain) {
			return sha1($chain->next($self, $params, $chain));
		});

		$expected = md5(sha1('Working?'));
		$result = $class::filteredMethod();
		$this->assertEqual($expected, $result);
	}
}

?>