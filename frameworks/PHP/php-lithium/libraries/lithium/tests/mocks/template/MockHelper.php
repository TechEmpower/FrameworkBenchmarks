<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\mocks\template;

class MockHelper extends \lithium\template\Helper {

	protected $_strings = array('link' => '<a href="{:url}"{:options}>{:title}</a>');

	/**
	 * Hack to expose protected properties for testing.
	 *
	 * @param string $property
	 * @return mixed
	 */
	public function __get($property) {
		return isset($this->{$property}) ? $this->{$property} : null;
	}

	public function testOptions($defaults, $options) {
		return $this->_options($defaults, $options);
	}

	public function testAttributes($params, $method = null, array $options = array()) {
		return $this->_attributes($params, $method, $options);
	}

	public function testRender($method, $string, $params, array $options = array()) {
		return $this->_render($method, $string, $params, $options);
	}
}

?>