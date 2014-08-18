<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\net\http;

/**
 * The `Route` class represents a single URL pattern which is matched against incoming requests, in
 * order to determine the correct controller and action that an HTTP request should be dispatched
 * to.
 *
 * Typically, `Route` objects are created and handled through the `lithium\net\http\Router` class,
 * as follows:
 *
 * {{{// This instantiates a Route object behind the scenes, and adds it to Router's collection:
 * Router::connect("/{:controller}/{:action}");
 *
 * // This matches a set of parameters against all Route objects contained in Router, and if a match
 * // is found, returns a string URL with parameters inserted into the URL pattern:
 * Router::match(array("controller" => "users", "action" => "login")); // returns "/users/login"
 * }}}
 *
 * For more advanced routing, however, you can directly instantiate a `Route` object, a subclass,
 * or any class that implements `parse()` and `match()` (see the documentation for each individual
 * method) and configure it manually -- if, for example, you want the route to match different
 * incoming URLs than it generates.
 *
 * {{{$route = new Route(array(
 *		'template' => '/users/{:user}',
 *		'pattern' => '@^/u(?:sers)?(?:/(?P<user>[^\/]+))$@',
 *		'params' => array('controller' => 'users', 'action' => 'index'),
 *		'match' => array('controller' => 'users', 'action' => 'index'),
 *		'defaults' => array('controller' => 'users'),
 *		'keys' => array('user' => 'user'),
 *		'options' => array('compile' => false, 'wrap' => false)
 * ));
 * Router::connect($route); // this will match '/users/<username>' or '/u/<username>'.
 * }}}
 *
 * For additional information on the `'options'` constructor key, see
 * `lithium\net\http\Route::compile()`. To learn more about Lithium's routing system, see
 * `lithium\net\http\Router`.
 *
 * @see lithium\net\http\Route::compile()
 * @see lithium\net\http\Router
 */
class Route extends \lithium\core\Object {

	/**
	 * The URL template string that the route matches.
	 *
	 * This string can contain fixed elements, i.e. `"/admin"`, capture elements,
	 * i.e. `"/{:controller}"`, capture elements optionally paired with regular expressions or
	 * named regular expression patterns, i.e. `"/{:id:\d+}"` or `"/{:id:ID}"`, the special wildcard
	 * capture, i.e. `"{:args}"`, or any combination thereof, i.e.
	 * `"/admin/{:controller}/{:id:\d+}/{:args}"`.
	 *
	 * @var string
	 */
	protected $_template = '';

	/**
	 * The regular expression used to match URLs.
	 *
	 * This regular expression is typically 'compiled' down from the higher-level syntax used in
	 * `$_template`, but can be set manually with compilation turned off in the constructor for
	 * extra control or if you are using pre-compiled `Route` objects.
	 *
	 * @var string
	 * @see lithium\net\http\Route::$_template
	 * @see lithium\net\http\Route::__construct()
	 */
	protected $_pattern = '';

	/**
	 * An array of route parameter names (i.e. {:foo}) that appear in the URL template.
	 *
	 * @var array
	 * @see lithium\net\http\Route::$_template
	 */
	protected $_keys = array();

	/**
	 * An array of key/value pairs representing the parameters of the route. For keys which match
	 * parameters present in the route template, the corresponding values match the default values
	 * of those parameters. Specifying a default value for a template parameter makes that
	 * parameter optional. Any other pairs specified must match exactly when doing a reverse lookup
	 * in order for the route to match.
	 *
	 * @var array
	 */
	protected $_params = array();

	/**
	 * The array of values that appear in the second parameter of `Router::connect()`, which are
	 * **not** present in the URL template. When matching a route, these parameters must appear
	 * **exactly** as specified here.
	 *
	 * @var array
	 */
	protected $_match = array();

	/**
	 * An array of metadata parameters which must be present in the request in order for the route
	 * to match.
	 *
	 * @var array
	 */
	protected $_meta = array();

	/**
	 * The default values for the keys present in the URL template.
	 *
	 * @var array
	 * @see lithium\net\http\Route::$_template
	 * @see lithium\net\http\Route::$_keys
	 */
	protected $_defaults = array();

	/**
	 * An array of regular expression patterns used in route matching.
	 *
	 * @var array
	 */
	protected $_subPatterns = array();

	/**
	 * An array of parameter names which will persist by default when generating URLs. By default,
	 * the `'controller'` parameter is set to persist, which means that the controller name matched
	 * for a given request will be used to generate all URLs for that request, unless the
	 * `'controller'` parameter is specified in that URL with another value.
	 *
	 * @var array
	 */
	protected $_persist = array();

	/**
	 * Contains a function which will be executed if this route is matched. The function takes the
	 * instance of the associated `Request` object, and the array of matched route parameters, and
	 * must return either the parameters array (which may be modified by the handler) or a
	 * `Response` object, in which case the response will be returned directly. This may be used to
	 * handle redirects, or simple API services.
	 *
	 * @var object
	 */
	protected $_handler = null;

	/**
	 * Array of closures used to format route parameters when compiling URLs.
	 *
	 * @see lithium\net\http\Router::formatters()
	 * @var array
	 */
	protected $_formatters = array();

	/**
	 * Auto configuration properties. Also used as the list of properties to return when exporting
	 * this `Route` object to an array.
	 *
	 * @see lithium\net\http\Route::export()
	 * @var array
	 */
	protected $_autoConfig = array(
		'template', 'pattern', 'params', 'match', 'meta',
		'keys', 'defaults', 'subPatterns', 'persist', 'handler'
	);

	public function __construct(array $config = array()) {
		$defaults = array(
			'params'   => array(),
			'template' => '/',
			'pattern'  => '',
			'match'    => array(),
			'meta'     => array(),
			'defaults' => array(),
			'keys'     => array(),
			'persist'  => array(),
			'handler'  => null,
			'continue' => false,
			'formatters' => array(),
			'unicode'  => true
		);
		parent::__construct($config + $defaults);
	}

	protected function _init() {
		parent::_init();

		if (!$this->_config['continue'] && !preg_match('@{:action:.*?}@', $this->_template)) {
			$this->_params += array('action' => 'index');
		}
		if (!$this->_config['pattern']) {
			$this->compile();
		}
		if ($isKey = isset($this->_keys['controller']) || isset($this->_params['controller'])) {
			$this->_persist = $this->_persist ?: array('controller');
		}
	}

	/**
	 * Attempts to parse a request object and determine its execution details.
	 *
	 * @param object $request A request object, usually an instance of `lithium\net\http\Request`,
	 *        containing the details of the request to be routed.
	 * @param array $options Used to determine the operation of the method, and override certain
	 *              values in the `Request` object:
	 *              - `'url'` _string_: If present, will be used to match in place of the `$url`
	 *                 property of `$request`.
	 * @return mixed If this route matches `$request`, returns an array of the execution details
	 *         contained in the route, otherwise returns false.
	 */
	public function parse($request, array $options = array()) {
		$defaults = array('url' => $request->url);
		$options += $defaults;
		$url = '/' . trim($options['url'], '/');
		$pattern = $this->_pattern;

		if (!preg_match($pattern, $url, $match)) {
			return false;
		}
		foreach ($this->_meta as $key => $compare) {
			$value = $request->get($key);

			if (!($compare == $value || (is_array($compare) && in_array($value, $compare)))) {
				return false;
			}
		}
		if (isset($match['args'])) {
			$match['args'] = explode('/', $match['args']);
		}
		$result = array_filter(array_intersect_key($match, $this->_keys));
		if (isset($this->_keys['args'])) {
			$result += array('args' => array());
		}
		$result += $this->_params + $this->_defaults;
		$request->params = $result + (array) $request->params;
		$request->persist = array_unique(array_merge($request->persist, $this->_persist));

		if ($this->_handler) {
			$handler = $this->_handler;
			return $handler($request);
		}
		return $request;
	}

	/**
	 * Matches a set of parameters against the route, and returns a URL string if the route matches
	 * the parameters, or false if it does not match.
	 *
	 * @param array $options
	 * @param string $context
	 * @return mixed
	 */
	public function match(array $options = array(), $context = null) {
		$defaults = array('action' => 'index');
		$query = null;

		if (!$this->_config['continue']) {
			$options += $defaults;

			if (isset($options['?'])) {
				$query = $options['?'];
				$query = '?' . (is_array($query) ? http_build_query($query) : $query);
				unset($options['?']);
			}
		}

		if (!$options = $this->_matchKeys($options)) {
			return false;
		}
		foreach ($this->_subPatterns as $key => $pattern) {
			if (isset($options[$key]) && !preg_match("/^{$pattern}$/", $options[$key])) {
				return false;
			}
		}
		$defaults = $this->_defaults + $defaults;

		if ($this->_config['continue']) {
			return $this->_write(array('args' => '{:args}') + $options, $this->_defaults);
		}
		return $this->_write($options, $defaults + array('args' => '')) . $query;
	}

	/**
	 * Returns a boolean value indicating whether this is a continuation route. If `true`, this
	 * route will allow incoming requests to "fall through" to other routes, aggregating parameters
	 * for both this route and any subsequent routes.
	 *
	 * @return boolean Returns the value of `$_config['continue']`.
	 */
	public function canContinue() {
		return $this->_config['continue'];
	}

	/**
	 * A helper method used by `match()` to verify that options required to match this route are
	 * present in a URL array.
	 *
	 * @see lithium\net\http\Route::match()
	 * @param array $options An array of URL parameters.
	 * @return mixed On success, returns an updated array of options, merged with defaults. On
	 *         failure, returns `false`.
	 */
	protected function _matchKeys($options) {
		$args = array('args' => 'args');

		if (array_intersect_key($options, $this->_match) != $this->_match) {
			return false;
		}
		if ($this->_config['continue']) {
			if (array_intersect_key($this->_keys, $options + $args) != $this->_keys) {
				return false;
			}
		} else {
			if (array_diff_key(array_diff_key($options, $this->_match), $this->_keys) !== array()) {
				return false;
			}
		}
		$options += $this->_defaults;
		$base = $this->_keys + $args;
		$match = array_intersect_key($this->_keys, $options) + $args;
		sort($base);
		sort($match);

		if ($base !== $match) {
			return false;
		}
		return $options;
	}

	/**
	 * Writes a set of URL options to this route's template string.
	 *
	 * @param array $options The options to write to this route, with defaults pre-merged.
	 * @param array $defaults The default template options for this route (contains hard-coded
	 *        default values).
	 * @return string Returns the route template string with option values inserted.
	 */
	protected function _write($options, $defaults) {
		$template = $this->_template;
		$trimmed = true;
		$options += array('args' => '');

		foreach (array_reverse($this->_keys, true) as $key) {
			$value =& $options[$key];
			$pattern = isset($this->_subPatterns[$key]) ? ":{$this->_subPatterns[$key]}" : '';
			$rpl = "{:{$key}{$pattern}}";
			$len = strlen($rpl) * -1;

			if ($trimmed && isset($defaults[$key]) && $value == $defaults[$key]) {
				if (substr($template, $len) == $rpl) {
					$template = rtrim(substr($template, 0, $len), '/');
					continue;
				}
			}
			if (isset($this->_config['formatters'][$key])) {
				$value = $this->_config['formatters'][$key]($value);
			}
			if ($value === null) {
				$template = str_replace("/{$rpl}", '', $template);
				continue;
			}
			if ($key !== 'args') {
				$trimmed = false;
			}
			$template = str_replace($rpl, $value, $template);
		}
		return $template ?: '/';
	}

	/**
	 * Exports the properties that make up the route to an array, for debugging, caching or
	 * introspection purposes.
	 *
	 * @return array An array containing the properties of the route object, such as URL templates
	 *         and parameter lists.
	 */
	public function export() {
		$result = array();

		foreach ($this->_autoConfig as $key) {
			if ($key === 'formatters') {
				continue;
			}
			$result[$key] = $this->{'_' . $key};
		}
		return $result;
	}

	/**
	 * Compiles URL templates into regular expression patterns for matching against request URLs,
	 * and extracts template parameters into match-parameter arrays.
	 *
	 * @return void
	 */
	public function compile() {
		$this->_match = $this->_params;

		foreach ($this->_params as $key => $value) {
			if (!strpos($key, ':')) {
				continue;
			}
			unset($this->_params[$key]);
			$this->_meta[$key] = $value;
		}

		if ($this->_template === '/' || $this->_template === '') {
			$this->_pattern = '@^/*$@';
			return;
		}
		$this->_pattern = "@^{$this->_template}\$@";
		$match = '@([/.])?\{:([^:}]+):?((?:[^{]+?(?:\{[0-9,]+\})?)*?)\}@S';

		if ($this->_config['unicode']) {
			$this->_pattern .= 'u';
		}
		preg_match_all($match, $this->_pattern, $m);

		if (!$tokens = $m[0]) {
			return;
		}
		$slashes = $m[1];
		$params = $m[2];
		$regexs = $m[3];
		unset($m);
		$this->_keys = array();

		foreach ($params as $i => $param) {
			$this->_keys[$param] = $param;
			$this->_pattern = $this->_regex($regexs[$i], $param, $tokens[$i], $slashes[$i]);
		}
		$this->_defaults = array_intersect_key($this->_params, $this->_keys);
		$this->_match = array_diff_key($this->_params, $this->_defaults);
	}

	/**
	 * Generates a sub-expression capture group for a route regex, using an optional user-supplied
	 * matching pattern.
	 *
	 * @param string $regex An optional user-supplied match pattern. If a route is defined like
	 *               `"/{:id:\d+}"`, then the value will be `"\d+"`.
	 * @param string $param The parameter name which the capture group is assigned to, i.e.
	 *               `'controller'`, `'id'` or `'args'`.
	 * @param string $token The full token representing a matched element in a route template, i.e.
	 *               `'/{:action}'`, `'/{:path:js|css}'`, or `'.{:type}'`.
	 * @param string $prefix The prefix character that separates the parameter from the other
	 *               elements of the route. Usually `'.'` or `'/'`.
	 * @return string Returns the full route template, with the value of `$token` replaced with a
	 *         generated regex capture group.
	 */
	protected function _regex($regex, $param, $token, $prefix) {
		if ($regex) {
			$this->_subPatterns[$param] = $regex;
		} elseif ($param == 'args') {
			$regex = '.*';
		} else {
			$regex = '[^\/]+';
		}

		$req = $param === 'args' || array_key_exists($param, $this->_params) ? '?' : '';

		if ($prefix === '/') {
			$pattern = "(?:/(?P<{$param}>{$regex}){$req}){$req}";
		} elseif ($prefix === '.') {
			$pattern = "\\.(?P<{$param}>{$regex}){$req}";
		} else {
			$pattern = "(?P<{$param}>{$regex}){$req}";
		}
		return str_replace($token, $pattern, $this->_pattern);
	}
}

?>