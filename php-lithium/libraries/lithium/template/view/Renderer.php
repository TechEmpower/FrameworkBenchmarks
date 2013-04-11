<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\template\view;

use RuntimeException;
use lithium\core\Libraries;
use lithium\core\ClassNotFoundException;

/**
 * The `Renderer` abstract class serves as a base for all concrete `Renderer` adapters.
 *
 * When in a view, the local scope is that of an instance of `Renderer` - meaning that
 * `$this` in views is an instance of the current renderer adapter.
 *
 * For more information about implementing your own template loaders or renderers, see the
 * `lithium\template\View` class.
 *
 * @see lithium\template\View
 * @see lithium\template\adapter\File
 * @see lithium\template\adapter\Simple
 */
abstract class Renderer extends \lithium\core\Object {

	/**
	 * These configuration variables will automatically be assigned to their corresponding protected
	 * properties when the object is initialized.
	 *
	 * @var array
	 */
	protected $_autoConfig = array(
		'request', 'response', 'context', 'strings', 'handlers', 'view', 'classes' => 'merge'
	);

	/**
	 * Holds an instance of the `View` object that created this rendering context. See the `view()`
	 * method for more details.
	 *
	 * @see lithium\template\view\Renderer::view()
	 * @var object
	 */
	protected $_view = null;

	/**
	 * Context values that exist across all templates rendered in this context.  These values
	 * are usually rendered in the layout template after all other values have rendered.
	 *
	 * @var array
	 */
	protected $_context = array(
		'content' => '', 'title' => '', 'scripts' => array(), 'styles' => array(), 'head' => array()
	);

	/**
	 * `Renderer`'s dependencies. These classes are used by the output handlers to generate URLs
	 * for dynamic resources and static assets.
	 *
	 * @see Renderer::$_handlers
	 * @var array
	 */
	protected $_classes = array(
		'router' => 'lithium\net\http\Router',
		'media'  => 'lithium\net\http\Media'
	);

	/**
	 * Contains the list of helpers currently in use by this rendering context. Helpers are loaded
	 * via the `helper()` method, which is called by `Renderer::__get()`, allowing for on-demand
	 * loading of helpers.
	 *
	 * @var array
	 */
	protected $_helpers = array();

	/**
	 * Aggregates named string templates used by helpers. Can be overridden to change the default
	 * strings a helper uses.
	 *
	 * @var array
	 */
	protected $_strings = array();

	/**
	 * The `Request` object instance, if applicable.
	 *
	 * @var object The request object.
	 */
	protected $_request = null;

	/**
	 * The `Response` object instance, if applicable.
	 *
	 * @var object The response object.
	 */
	protected $_response = null;

	/**
	 * Automatically matches up template strings by name to output handlers.  A handler can either
	 * be a string, which represents a method name of the helper, or it can be a closure or callable
	 * object.  A handler takes 3 parameters: the value to be filtered, the name of the helper
	 * method that triggered the handler, and the array of options passed to the `_render()`. These
	 * handlers are shared among all helper objects, and are automatically triggered whenever a
	 * helper method renders a template string (using `_render()`) and a key which is to be embedded
	 * in the template string matches an array key of a corresponding handler.
	 *
	 * @see lithium\template\view\Renderer::applyHandler()
	 * @see lithium\template\view\Renderer::handlers()
	 * @var array
	 */
	protected $_handlers = array();

	/**
	 * An array containing any additional variables to be injected into view templates. This allows
	 * local variables to be communicated between multiple templates (i.e. an element and a layout)
	 * which are using the same rendering context.
	 *
	 * @see lithium\template\view\Renderer::set()
	 * @var array
	 */
	protected $_data = array();

	/**
	 * Variables that have been set from a view/element/layout/etc. that should be available to the
	 * same rendering context.
	 *
	 * @var array Key/value pairs of variables
	 */
	protected $_vars = array();

	/**
	 * Available options accepted by `template\View::render()`, used when rendering.
	 *
	 * @see lithium\template\View::render()
	 * @var array
	 */
	protected $_options = array();

	/**
	 * Render the template with given data. Abstract; must be added to subclasses.
	 *
	 * @param string $template
	 * @param array|string $data
	 * @param array $options
	 * @return string Returns the result of the rendered template.
	 */
	abstract public function render($template, $data = array(), array $options = array());

	/**
	 * Renderer constructor.
	 *
	 * Accepts these following configuration parameters:
	 * - `view`: The `View` object associated with this renderer.
	 * - `strings`: String templates used by helpers.
	 * - `handlers`: An array of output handlers for string template inputs.
	 * - `request`: The `Request` object associated with this renderer and passed to the
	 *              defined handlers.
	 * - `response`: The `Response` object associated with this renderer.
	 * - `context`: An array of the current rendering context data, including `content`,
	 *              `title`, `scripts`, `head` and `styles`.
	 * @param array $config
	 */
	public function __construct(array $config = array()) {
		$defaults = array(
			'view' => null,
			'strings' => array(),
			'handlers' => array(),
			'request' => null,
			'response' => null,
			'context' => array(
				'content' => '', 'title' => '', 'scripts' => array(),
				'styles' => array(), 'head' => array()
			)
		);
		parent::__construct((array) $config + $defaults);
	}

	/**
	 * Sets the default output handlers for string template inputs.
	 *
	 * @return void
	 */
	protected function _init() {
		parent::_init();

		$request =& $this->_request;
		$context =& $this->_context;
		$classes =& $this->_classes;
		$h = $this->_view ? $this->_view->outputFilters['h'] : null;

		$this->_handlers += array(
			'url' => function($url, $ref, array $options = array()) use (&$classes, &$request, $h) {
				$url = $classes['router']::match($url ?: '', $request, $options);
				return $h ? str_replace('&amp;', '&', $h($url)) : $url;
			},
			'path' => function($path, $ref, array $options = array()) use (&$classes, &$request) {
				$defaults = array('base' => $request ? $request->env('base') : '');
				$type = 'generic';

				if (is_array($ref) && $ref[0] && $ref[1]) {
					list($helper, $methodRef) = $ref;
					list($class, $method) = explode('::', $methodRef);
					$type = $helper->contentMap[$method];
				}
				return $classes['media']::asset($path, $type, $options + $defaults);
			},
			'options' => '_attributes',
			'title'   => 'escape',
			'value'   => 'escape',
			'scripts' => function($scripts) use (&$context) {
				return "\n\t" . join("\n\t", $context['scripts']) . "\n";
			},
			'styles' => function($styles) use (&$context) {
				return "\n\t" . join("\n\t", $context['styles']) . "\n";
			},
			'head' => function($head) use (&$context) {
				return "\n\t" . join("\n\t", $context['head']) . "\n";
			}
		);
		unset($this->_config['view']);
	}

	/**
	 * Magic `__isset` method.
	 *
	 * Is triggered by calling isset() or empty() on inaccessible properties, and performs
	 * an `isset()` check on for keys in the current `context`.
	 *
	 * @param string $property The accessed property.
	 * @return boolean True if set, false otherwise.
	 */
	public function __isset($property) {
		return isset($this->_context[$property]);
	}

	/**
	 * Returns a helper object or context value by name.
	 *
	 * @param string $property The name of the helper or context value to return.
	 * @return mixed
	 * @filter
	 */
	public function __get($property) {
		$context = $this->_context;
		$helpers = $this->_helpers;

		$filter = function($self, $params, $chain) use ($context, $helpers) {
			$property = $params['property'];

			foreach (array('context', 'helpers') as $key) {
				if (isset(${$key}[$property])) {
					return ${$key}[$property];
				}
			}
			return $self->helper($property);
		};
		return $this->_filter(__METHOD__, compact('property'), $filter);
	}

	/**
	 * Dispatches method calls for (a) rendering context values or (b) applying handlers to pieces
	 * of content. If `$method` is a key in `Renderer::$_context`, the corresponding context value
	 * will be returned (with the value run through a matching handler if one is available). If
	 * `$method` is a key in `Renderer::$_handlers`, the value passed as the first parameter in the
	 * method call will be passed through the handler and returned.
	 *
	 * @see lithium\template\view\Renderer::$_context
	 * @see lithium\template\view\Renderer::$_handlers
	 * @see lithium\template\view\Renderer::applyHandler()
	 * @param string $method The method name to call, usually either a rendering context value or a
	 *               content handler.
	 * @param array $params
	 * @return mixed
	 */
	public function __call($method, $params) {
		if (!isset($this->_context[$method]) && !isset($this->_handlers[$method])) {
			return isset($params[0]) ? $params[0] : null;
		}
		if (!isset($this->_handlers[$method]) && !$params) {
			return $this->_context[$method];
		}
		if (isset($this->_context[$method]) && $params) {
			if (is_array($this->_context[$method])) {
				$this->_context[$method][] = $params[0];
			} else {
				$this->_context[$method] = $params[0];
			}
		}
		if (!isset($this->_context[$method])) {
			$params += array(null, array());
			return $this->applyHandler(null, null, $method, $params[0], $params[1]);
		}
		return $this->applyHandler(null, null, $method, $this->_context[$method]);
	}

	/**
	 * Custom check to determine if our given magic methods can be responded to.
	 *
	 * @param  string  $method     Method name.
	 * @param  bool    $internal   Interal call or not.
	 * @return bool
	 */
	public function respondsTo($method, $internal = false) {
		return is_callable(array($this, $method), true);
	}

	/**
	 * Brokers access to helpers attached to this rendering context, and loads helpers on-demand if
	 * they are not available.
	 *
	 * @param string $name Helper name
	 * @param array $config
	 * @return object
	 */
	public function helper($name, array $config = array()) {
		if (isset($this->_helpers[$name])) {
			return $this->_helpers[$name];
		}
		try {
			$config += array('context' => $this);
			return $this->_helpers[$name] = Libraries::instance('helper', ucfirst($name), $config);
		} catch (ClassNotFoundException $e) {
			if (ob_get_length()) {
				ob_end_clean();
			}
			throw new RuntimeException("Helper `{$name}` not found.");
		}
	}

	/**
	 * Manages template strings.
	 *
	 * @param mixed $strings
	 * @return mixed
	 */
	public function strings($strings = null) {
		if (is_array($strings)) {
			return $this->_strings = $this->_strings + $strings;
		}
		if (is_string($strings)) {
			return isset($this->_strings[$strings]) ? $this->_strings[$strings] : null;
		}
		return $this->_strings;
	}

	/**
	 * Returns either one or all context values for this rendering context. Context values persist
	 * across all templates rendered in the current context, and are usually outputted in a layout
	 * template.
	 *
	 * @see lithium\template\view\Renderer::$_context
	 * @param string $property If unspecified, an associative array of all context values is
	 *               returned. If a string is specified, the context value matching the name given
	 *               will be returned, or `null` if that name does not exist.
	 * @return mixed A string or array, depending on whether `$property` is specified.
	 */
	public function context($property = null) {
		if ($property) {
			return isset($this->_context[$property]) ? $this->_context[$property] : null;
		}
		return $this->_context;
	}

	/**
	 * Gets or adds content handlers from/to this rendering context, depending on the value of
	 * `$handlers`.  For more on how to implement handlers and the various types, see
	 * `applyHandler()`.
	 *
	 * @see lithium\template\view\Renderer::applyHandler()
	 * @see lithium\template\view\Renderer::$_handlers
	 * @param mixed $handlers If `$handlers` is empty or no value is provided, the current list
	 *              of handlers is returned.  If `$handlers` is a string, the handler with the name
	 *              matching the string will be returned, or null if one does not exist. If
	 *              `$handlers` is an array, the handlers named in the array will be merged into
	 *              the list of handlers in this rendering context, with the pre-existing handlers
	 *              taking precedence over those newly added.
	 * @return mixed Returns an array of handlers or a single handler reference, depending on the
	 *               value of `$handlers`.
	 */
	public function handlers($handlers = null) {
		if (is_array($handlers)) {
			return $this->_handlers += $handlers;
		}
		if (is_string($handlers)) {
			return isset($this->_handlers[$handlers]) ? $this->_handlers[$handlers] : null;
		}
		return $this->_handlers;
	}

	/**
	 * Filters a piece of content through a content handler.  A handler can be:
	 * - a string containing the name of a method defined in `$helper`. The method is called with 3
	 *   parameters: the value to be handled, the helper method called (`$method`) and the
	 *   `$options` that were passed into `applyHandler`.
	 * - an array where the first element is an object reference, and the second element is a method
	 *   name.  The method name given will be called on the object with the same parameters as
	 *   above.
	 * - a closure, which takes the value as the first parameter, an array containing an instance of
	 *   the calling helper and the calling method name as the second, and `$options` as the third.
	 * In all cases, handlers should return the transformed version of `$value`.
	 *
	 * @see lithium\template\view\Renderer::handlers()
	 * @see lithium\template\view\Renderer::$_handlers
	 * @param object $helper The instance of the object (usually a helper) that is invoking
	 * @param string $method The object (helper) method which is applying the handler to the content
	 * @param string $name The name of the value to which the handler is applied, i.e. `'url'`,
	 *               `'path'` or `'title'`.
	 * @param mixed $value The value to be transformed by the handler, which is ultimately returned.
	 * @param array $options Any options which should be passed to the handler used in this call.
	 * @return mixed The transformed value of `$value`, after it has been processed by a handler.
	 */
	public function applyHandler($helper, $method, $name, $value, array $options = array()) {
		if (!(isset($this->_handlers[$name]) && $handler = $this->_handlers[$name])) {
			return $value;
		}

		switch (true) {
			case is_string($handler) && !$helper:
				$helper = $this->helper('html');
			case is_string($handler) && is_object($helper):
				return $helper->invokeMethod($handler, array($value, $method, $options));
			case is_array($handler) && is_object($handler[0]):
				list($object, $func) = $handler;
				return $object->invokeMethod($func, array($value, $method, $options));
			case is_callable($handler):
				return $handler($value, array($helper, $method), $options);
			default:
				return $value;
		}
	}

	/**
	 * Returns the `Request` object associated with this rendering context.
	 *
	 * @return object Returns an instance of `lithium\action\Request`, which provides the context
	 *         for URLs, etc. which are generated in any templates rendered by this context.
	 */
	public function request() {
		return $this->_request;
	}

	/**
	 * Returns the `Response` object associated with this rendering context.
	 *
	 * @return object Returns an instance of `lithium\action\Response`, which provides the i.e.
	 *         the encoding for the document being the result of templates rendered by this context.
	 */
	public function response() {
		return $this->_response;
	}

	/**
	 * Retuns the `View` object that controls this rendering context's instance. This can be used,
	 * for example, to render view elements, i.e. `<?=$this->view()->render('element' $name); ?>`.
	 *
	 * @return void
	 */
	public function view() {
		return $this->_view;
	}

	/**
	 * Returns all variables and their values that have been set.
	 *
	 * @return array Key/value pairs of data that has been set.
	 */
	public function data() {
		return $this->_data + $this->_vars;
	}

	/**
	 * Allows variables to be set by one template and used in subsequent templates rendered using
	 * the same context. For example, a variable can be set in a template and used in an element
	 * rendered within a template, or an element or template could set a variable which would be
	 * made available in the layout.
	 *
	 * @param array $data An array of key/value pairs representing local variables that should be
	 *              made available to all other templates rendered in this rendering context.
	 * @return void
	 */
	public function set(array $data = array()) {
		$this->_data = $data + $this->_data;
		$this->_vars = $data + $this->_vars;
	}

	/**
	 * Shortcut method used to render elements and other nested templates from inside the templating
	 * layer.
	 *
	 * @see lithium\template\View::$_processes
	 * @see lithium\template\View::render()
	 * @param string $type The type of template to render, usually either `'element'` or
	 *               `'template'`. Indicates the process used to render the content. See
	 *               `lithium\template\View::$_processes` for more info.
	 * @param string $template The template file name. For example, if `'header'` is passed, and
	 *               `$type` is set to `'element'`, then the template rendered will be
	 *               `views/elements/header.html.php` (assuming the default configuration).
	 * @param array $data An array of any other local variables that should be injected into the
	 *              template. By default, only the values used to render the current template will
	 *              be sent. If `$data` is non-empty, both sets of variables will be merged.
	 * @param array $options Any options accepted by `template\View::render()`.
	 * @return string Returns a the rendered template content as a string.
	 */
	protected function _render($type, $template, array $data = array(), array $options = array()) {
		$options += $this->_options;
		return $this->_view->render($type, $data + $this->_data, compact('template') + $options);
	}
}

?>