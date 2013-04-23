<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\template;

use lithium\tests\mocks\template\MockHelper;
use lithium\tests\mocks\template\MockRenderer;

class HelperTest extends \lithium\test\Unit {

	public function setUp() {
		$this->helper = new MockHelper();
	}

	/**
	 * Tests that constructor parameters are properly assigned to protected properties.
	 *
	 * @return void
	 */
	public function testObjectConstructionWithParameters() {
		$this->assertNull($this->helper->_context);

		$params = array(
			'context' => new MockRenderer(),
			'handlers' => array('content' => function($value) { return "\n{$value}\n"; })
		);
		$helper = new MockHelper($params);
		$this->assertEqual($helper->_context, $params['context']);
	}

	/**
	 * Tests the default escaping for HTML output.  When implementing helpers that do not output
	 * HTML/XML, the `escape()` method should be overridden accordingly.
	 *
	 * @return void
	 */
	public function testDefaultEscaping() {
		$result = $this->helper->escape('<script>alert("XSS!");</script>');
		$expected = '&lt;script&gt;alert(&quot;XSS!&quot;);&lt;/script&gt;';
		$this->assertEqual($expected, $result);

		$result = $this->helper->escape('<script>//alert("XSS!");</script>', null, array(
			'escape' => false
		));
		$expected = '<script>//alert("XSS!");</script>';
		$this->assertEqual($expected, $result);

		$result = $this->helper->escape(array(
			'<script>alert("XSS!");</script>', '<script>alert("XSS!");</script>'
		));
		$expected = array(
			'&lt;script&gt;alert(&quot;XSS!&quot;);&lt;/script&gt;',
			'&lt;script&gt;alert(&quot;XSS!&quot;);&lt;/script&gt;'
		);
		$this->assertEqual($expected, $result);
	}

	/**
	 * Tests unescaped values passed through the escape() method. Unescaped values
	 * should be returned exactly the same as the original value.
	 *
	 * @return void
	 */
	public function testUnescapedValue() {
		$value  = '<blockquote>"Thou shalt not escape!"</blockquote>';
		$result = $this->helper->escape($value, null, array('escape' => false));
		$this->assertEqual($value, $result);
	}

	public function testOptions() {
		$defaults = array('value' => null);
		$options = array('value' => 1, 'title' => 'one');
		$expected = array(
			array('value' => 1, 'title' => 'one'),
			array('title' => 'one')
		);
		$result = $this->helper->testOptions($defaults, $options);
		$this->assertEqual($expected, $result);
	}

	public function testAttributes() {
		$attributes = array('value' => 1, 'title' => 'one');
		$expected = ' value="1" title="one"';
		$result = $this->helper->testAttributes($attributes);
		$this->assertEqual($expected, $result);

		$attributes = ' value="1" title="one"';
		$result = $this->helper->testAttributes('value="1" title="one"');
		$this->assertEqual($expected, $result);

		$attributes = array('checked' => true, 'title' => 'one');
		$expected = ' checked="checked" title="one"';
		$result = $this->helper->testAttributes($attributes);
		$this->assertEqual($expected, $result);

		$attributes = array('checked' => false);
		$result = $this->helper->testAttributes($attributes);
		$this->assertEqual('', $result);
	}

	public function testAttributeEscaping() {
		$attributes = array('checked' => true, 'title' => '<foo>');
		$expected = ' checked="checked" title="&lt;foo&gt;"';
		$result = $this->helper->testAttributes($attributes);
		$this->assertEqual($expected, $result);

		$attributes = array('checked' => true, 'title' => '<foo>');
		$expected = ' checked="checked" title="<foo>"';
		$result = $this->helper->testAttributes($attributes, null, array('escape' => false));
		$this->assertEqual($expected, $result);
	}

	public function testAttributeMinimization() {
		$attributes = array('selected' => 1);
		$expected = ' selected="selected"';
		$result = $this->helper->testAttributes($attributes);
		$this->assertEqual($expected, $result);

		$attributes = array('selected' => true);
		$expected = ' selected="selected"';
		$result = $this->helper->testAttributes($attributes);
		$this->assertEqual($expected, $result);

		$attributes = array('selected' => 'true');
		$expected = ' selected="true"';
		$result = $this->helper->testAttributes($attributes);
		$this->assertEqual($expected, $result);
	}

	public function testInstantiationWithNoContext() {
		$this->helper = new MockHelper();
		$result = $this->helper->testRender(null, "foo {:bar}", array('bar' => 'baz'));
		$this->assertEqual("foo baz", $result);
	}

	public function testRender() {
		$params = array(
			'context' => new MockRenderer(),
			'handlers' => array('content' => function($value) { return "\n{$value}\n"; })
		);
		$helper = new MockHelper($params);
		$config = array(
			'title' => 'cool',
			'url' => '/here',
			'options' => array('value' => 1, 'title' => 'one')
		);
		$expected = '<a href="/here" value="1" title="one">cool</a>';
		$result = $helper->testRender('link', 'link', $config);
		$this->assertEqual($expected, $result);

		$handlers = array('path' => function($path) { return "/webroot{$path}"; });
		$params = array('context' => new MockRenderer(compact('handlers')));
		$helper = new MockHelper($params);
		$handlers = array('url' => 'path');
		$expected = '<a href="/webroot/here" value="1" title="one">cool</a>';
		$result = $helper->testRender('link', 'link', $config, compact('handlers'));
		$this->assertEqual($expected, $result);
	}
}

?>