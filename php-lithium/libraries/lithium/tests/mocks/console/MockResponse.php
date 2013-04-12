<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\console;

use lithium\util\String;

class MockResponse extends \lithium\console\Response {

	public $testAction;

	public $testParam;

	public function __construct(array $config = array()) {
		parent::__construct($config);
		$this->output = null;
		$this->error = null;
	}

	public function output($output) {
		return $this->output .= String::insert($output, $this->styles(false));
	}

	public function error($error) {
		return $this->error .= String::insert($error, $this->styles(false));
	}

	public function __destruct() {
		$this->output = null;
		$this->error = null;
	}
}

?>