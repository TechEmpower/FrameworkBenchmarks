<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\template;

use lithium\util\String;

/**
 * Abstract class for template helpers to extend.
 * Supplies the basic functionality of _render and _options,
 * as well as escaping.
 *
 */
abstract class Helper extends \lithium\core\Object {

	/**
	 * Maps helper method names to content types as defined by the `Media` class, where key are
	 * method names, and values are the content type that the method name outputs a link to.
	 *
	 * @var array
	 */
	public $contentMap = array();

	/**
	 * Holds string templates which will be merged into the rendering context.
	 *
	 * @var array
	 */
	protected $_strings = array();

	/**
	 * The Renderer object this Helper is bound to.
	 *
	 * @var lithium\template\view\Renderer
	 * @see lithium\template\view\Renderer
	 */
	protected $_context = null;

	/**
	 * This property can be overwritten with any class dependencies a helper subclass has.
	 *
	 * @var array
	 */
	protected $_classes = array();

	/**
	 * Auto configuration properties.
	 *
	 * @var array
	 */
	protected $_autoConfig = array('classes' => 'merge', 'context');

	/**
	 * List of minimized HTML attributes.
	 *
	 * @var array
	 */
	protected $_minimized = array(
		'compact', 'checked', 'declare', 'readonly', 'disabled', 'selected', 'defer', 'ismap',
		'nohref', 'noshade', 'nowrap', 'multiple', 'noresize', 'async', 'autofocus'
	);

	public function __construct(array $config = array()) {
		$defaults = array('handlers' => array(), 'context' => null);
		parent::__construct($config + $defaults);
	}

	/**
	 * Imports local string definitions into rendering context.
	 *
	 * @return void
	 */
	protected function _init() {
		parent::_init();

		if (!$this->_context) {
			return;
		}
		$this->_context->strings($this->_strings);

		if ($this->_config['handlers']) {
			$this->_context->handlers($this->_config['handlers']);
		}
	}

	/**
	 * Escapes values according to the output type of the rendering context. Helpers that output to
	 * non-HTML/XML contexts should override this method accordingly.
	 *
	 * @param string $value
	 * @param mixed $method
	 * @param array $options
	 * @return mixed
	 */
	public function escape($value, $method = null, array $options = array()) {
		$defaults = array('escape' => true);
		$options += $defaults;

		if ($options['escape'] === false) {
			return $value;
		}
		if (is_array($value)) {
			return array_map(array($this, __FUNCTION__), $value);
		}
		return htmlspecialchars($value, ENT_QUOTES, 'UTF-8');
	}

	/**
	 * Takes the defaults and current options, merges them and returns options which have
	 * the default keys removed and full set of options as the scope.
	 *
	 * @param array $defaults
	 * @param array $scope the complete set of options
	 * @return array $scope, $options
	 */
	protected function _options(array $defaults, array $scope) {
		$scope += $defaults;
		$options = array_diff_key($scope, $defaults);
		return array($scope, $options);
	}

	/**
	 * Render a string template after applying context filters
	 * Use examples in the Html::link() method:
	 * `return $this->_render(__METHOD__, 'link', compact('title', 'url', 'options'), $scope);`
	 *
	 * @param string $method name of method that is calling the render (for context filters)
	 * @param string $string template key (in Helper::_strings) to render
	 * @param array $params associated array of template inserts {:key} will be replaced by value
	 * @param array $options Available options:
	 *              - `'handlers'` _array_: Before inserting `$params` inside the string template,
	 *              `$this->_context`'s handlers are applied to each value of `$params` according
	 *              to the key (e.g `$params['url']`, which is processed by the `'url'` handler
	 *              via `$this->_context->applyHandler()`).
	 *              The `'handlers'` option allow to set custom mapping beetween `$params`'s key and
	 *              `$this->_context`'s handlers. e.g. the following handler:
	 *              `'handlers' => array('url' => 'path')` will make `$params['url']` to be
	 *              processed by the `'path'` handler instead of the `'url'` one.
	 * @return string Rendered HTML
	 */
	protected function _render($method, $string, $params, array $options = array()) {
		$strings = $this->_strings;

		if ($this->_context) {
			foreach ($params as $key => $value) {
				$handler = isset($options['handlers'][$key]) ? $options['handlers'][$key] : $key;
				$params[$key] = $this->_context->applyHandler(
					$this, $method, $handler, $value, $options
				);
			}
			$strings = $this->_context->strings();
		}
		return String::insert(isset($strings[$string]) ? $strings[$string] : $string, $params);
	}

	/**
	 * Convert a set of options to HTML attributes
	 *
	 * @param array $params
	 * @param string $method
	 * @param array $options
	 * @return string
	 */
	protected function _attributes($params, $method = null, array $options = array()) {
		$defaults = array('escape' => true, 'prepend' => ' ', 'append' => '');
		$options += $defaults;
		$result = array();

		if (!is_array($params)) {
			return !$params ? '' : $options['prepend'] . $params;
		}
		foreach ($params as $key => $value) {
			if ($next = $this->_attribute($key, $value, $options)) {
				$result[] = $next;
			}
		}
		return $result ? $options['prepend'] . implode(' ', $result) . $options['append'] : '';
	}

	/**
	 * Convert a key/value pair to a valid HTML attribute.
	 *
	 * @param string $key The key name of the HTML attribute.
	 * @param mixed $value The HTML attribute value.
	 * @param array $options The options used when converting the key/value pair to attributes:
	 *              - `'escape'` _boolean_: Indicates whether `$key` and `$value` should be
	 *                HTML-escaped. Defaults to `true`.
	 *              - `'format'` _string_: The format string. Defaults to `'%s="%s"'`.
	 * @return string Returns an HTML attribute/value pair, in the form of `'$key="$value"'`.
	 */
	protected function _attribute($key, $value, array $options = array()) {
		$defaults = array('escape' => true, 'format' => '%s="%s"');
		$options += $defaults;

		if (in_array($key, $this->_minimized)) {
			$isMini = ($value == 1 || $value === true || $value == $key);
			if (!($value = $isMini ? $key : $value)) {
				return null;
			}
		}
		$value = (string) $value;

		if ($options['escape']) {
			return sprintf($options['format'], $this->escape($key), $this->escape($value));
		}
		return sprintf($options['format'], $key, $value);
	}
}

?>