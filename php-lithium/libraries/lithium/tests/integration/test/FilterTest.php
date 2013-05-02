<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\integration\test;

use lithium\test\Group;
use lithium\test\Report;

class FilterTest extends \lithium\test\Integration {

	/**
	 * Skip the tests if the xdebug extension is not loaded. The `Coverage`
	 * filter depends on the extension.
	 */
	public function skip() {
		$this->skipIf(!extension_loaded('xdebug'), 'The `xdebug` extension is not loaded.');
	}

	public function setUp() {
		$this->report = new Report(array(
			'title' => 'lithium\tests\mocks\test\MockFilterTest',
			'group' => new Group(
				array('data' => array('lithium\tests\mocks\test\MockFilterClassTest'))
			)
		));
	}

	public function testSingleTest() {
		$this->report->filters(array('Coverage' => null));

		$this->report->run();

		$filter = $this->report->results['filters']['lithium\test\filter\Coverage'];
		$data = $filter['lithium\tests\mocks\test\MockFilterClass'];
		$result = $data['percentage'];

		$this->assertTrue($result > 65 && $result < 70);
	}

	public function testSingleTestWithMultipleFilters() {
		$all = array(
			'Coverage',
			'Complexity',
			'Profiler',
			'Affected'
		);
		$permutations = $this->_powerPerms($all);

		foreach ($permutations as $filters) {
			$filters = array_flip($filters);
			$filters = array_map(function($v) {
				return "";
			}, $filters);

			$report = new Report(array(
				'title' => 'lithium\tests\mocks\test\MockFilterTest',
				'group' => new Group(
					array('data' => array('lithium\tests\mocks\test\MockFilterClassTest'))
				)
			));

			$report->filters($filters);

			$report->run();

			if (array_key_exists("Coverage", $filters)) {
				$expected = 40;

				$result = $report->results['filters'];

				$message = "Filter(s): '" . join(array_keys($filters), ", ") . "'";
				$message .= "returned no Coverage results.";
				$this->assertTrue(isset($result['lithium\test\filter\Coverage']), $message);
				$percentage = $result['lithium\test\filter\Coverage'];
				$percentage = $percentage['lithium\tests\mocks\test\MockFilterClass'];
				$percentage = $percentage['percentage'];

				$this->assertEqual($expected, $percentage);
			}
		}
	}

	/**
	 * Methods for getting all permutations of each set in the power set of an
	 * array of strings (from the php.net manual on shuffle).
	 *
	 * @todo This needs further refactoring.
	 */

	protected function _powerPerms($arr) {
		$powerSet = $this->_powerSet($arr);
		$result = array();

		foreach ($powerSet as $set) {
			$perms = $this->_perms($set);
			$result = array_merge($result,$perms);
		}
		return $result;
	}

	protected function _powerSet($in, $minLength = 1) {
		$count = count($in);
		$members = pow(2, $count);
		$return = array();

		for ($i = 0; $i < $members; $i++) {
			$b = sprintf("%0{$count}b", $i);
			$out = array();

			for ($j = 0; $j < $count; $j++) {
				if ($b[$j] === '1') {
					$out[] = $in[$j];
				}
			}
			if (count($out) >= $minLength) {
				$return[] = $out;
			}
		}
		return $return;
	}

	protected function _factorial($int) {
		if (!$int) {
			return 1;
		}
		$f = $int;
		while ($int > 1) {
			$f *= --$int;
		}
		return $f;
	}

	protected function _perm($arr, $nth = null) {
		if ($nth === null) {
			return $this->_perms($arr);
		}
		$result = array();
		$length = count($arr);

		while ($length--) {
			$f = $this->_factorial($length);
			$p = floor($nth / $f);
			$result[] = $arr[$p];
			$this->_arrayDeleteByKey($arr, $p);
			$nth -= $p * $f;
		}
		$result = array_merge($result,$arr);
		return $result;
	}

	protected function _perms($arr) {
		$p = array();

		for ($i = 0; $i < $this->_factorial(count($arr)); $i++) {
			$p[] = $this->_perm($arr, $i);
		}
		return $p;
	}

	protected function _arrayDeleteByKey(&$array, $deleteKey, $useOldKeys = false) {
		unset($array[$deleteKey]);

		if (!$useOldKeys) {
			$array = array_values($array);
		}
		return true;
	}
}

?>