<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2012, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\action;

use lithium\util\String;
use lithium\util\Inflector;
use lithium\core\Libraries;
use lithium\action\DispatchException;
use lithium\core\ClassNotFoundException;

/**
 * `Dispatcher` is the outermost layer of the framework, responsible for both receiving the initial
 * HTTP request and sending back a response at the end of the request's life cycle.
 *
 * After either receiving or instantiating a `Request` object instance, the `Dispatcher` passes that
 * instance to the `Router`, which produces the parameters necessary to dispatch the request
 * (unless no route matches, in which case an exception is thrown).
 *
 * Using these parameters, the `Dispatcher` loads and instantiates the correct `Controller` object,
 * and passes it the `Request` object instance. The `Controller` returns a `Response` object to the
 * `Dispatcher`, where the headers and content are rendered and sent to the browser.
 *
 * @see lithium\net\http\Router
 * @see lithium\action\Request
 * @see lithium\action\Response
 * @see lithium\action\Controller
 */
class Dispatcher extends \lithium\core\StaticObject {

	/**
	 * Fully-namespaced router class reference.  Class must implement a `parse()` method,
	 * which must return an array with (at a minimum) 'controller' and 'action' keys.
	 *
	 * @see lithium\net\http\Router::parse()
	 * @var array
	 */
	protected static $_classes = array(
		'router' => 'lithium\net\http\Router'
	);

	/**
	 * Contains pre-process format strings for changing Dispatcher's behavior based on 'rules'.
	 *
	 * Each key in the array represents a 'rule'; if a key that matches the rule is present (and
	 * not empty) in a route, (i.e. the result of `lithium\net\http\Router::parse()`) then the
	 * rule's value will be applied to the route before it is dispatched.  When applying a rule, any
	 * array elements of the flag which are present in the route will be modified
	 * using a `lithium\util\String::insert()`-formatted string.  Alternatively,
	 * a callback can be used to do custom transformations other than the
	 * default `lithium\util\String::insert()`.
	 *
	 * For example, to implement action prefixes (i.e. `admin_index()`), set a rule named 'admin',
	 * with a value array containing a modifier key for the `action` element of a route, i.e.:
	 * `array('action' => 'admin_{:action}')`. Now, if the `'admin'` key is present and not empty
	 * in the parameters returned from routing, the value of `'action'` will be rewritten per the
	 * settings in the rule.
	 *
	 * Here's another example.  To support normalizing actions,
	 * set a rule named 'action' with a value array containing a callback that uses
	 * `lithium\util\Inflector` to camelize the action:
	 *
	 * {{{
	 * use lithium\action\Dispatcher;
	 * use lithium\util\Inflector;
	 *
	 * Dispatcher::config(array('rules' => array(
	 * 	'action' => array('action' => function($params) {
	 * 		return Inflector::camelize(strtolower($params['action']), false);
	 * 	})
	 * )));
	 * }}}
	 *
	 * The rules can be a callback as well:
	 *
	 * {{{
	 * Dispatcher::config(array('rules' => function($params) {
	 * 	if (isset($params['admin'])) {
	 * 		return array('special' => array('action' => 'special_{:action}'));
	 * 	}
	 * 	return array();
	 * }));
	 * }}}
	 *
	 * @see lithium\action\Dispatcher::config()
	 * @see lithium\util\String::insert()
	 */
	protected static $_rules = array();

	/**
	 * Used to set configuration parameters for the `Dispatcher`.
	 *
	 * @see lithium\action\Dispatcher::$_rules
	 * @param array $config Possible key settings are `'classes'` which sets the class dependencies
	 *              for `Dispatcher` (i.e. `'request'` or `'router'`) and `'rules'`, which sets the
	 *              pre-processing rules for routing parameters. For more information on the
	 *              `'rules'` setting, see the `$_rules` property.
	 * @return array If no parameters are passed, returns an associative array with the current
	 *         configuration, otherwise returns `null`.
	 */
	public static function config(array $config = array()) {
		if (!$config) {
			return array('rules' => static::$_rules);
		}

		foreach ($config as $key => $val) {
			$key = "_{$key}";
			if (!is_array($val)) {
				static::${$key} = $val;
				continue;
			}
			if (isset(static::${$key})) {
				static::${$key} = $val + static::${$key};
			}
		}
	}

	/**
	 * Dispatches a request based on a request object (an instance or subclass of
	 * `lithium\net\http\Request`).
	 *
	 * @see lithium\action\Request
	 * @see lithium\action\Response
	 * @param object $request An instance of a request object (usually `lithium\action\Request`)
	 *               with HTTP request information.
	 * @param array $options
	 * @return mixed Returns the value returned from the callable object retrieved from
	 *         `Dispatcher::_callable()`, which is either a string or an instance of
	 *         `lithium\action\Response`.
	 * @filter
	 */
	public static function run($request, array $options = array()) {
		$router = static::$_classes['router'];
		$params = compact('request', 'options');

		return static::_filter(__FUNCTION__, $params, function($self, $params) use ($router) {
			$request = $params['request'];
			$options = $params['options'];

			if (($result = $router::process($request)) instanceof Response) {
				return $result;
			}
			$params = $self::applyRules($result->params);

			if (!$params) {
				throw new DispatchException('Could not route request.');
			}
			$callable = $self::invokeMethod('_callable', array($result, $params, $options));
			return $self::invokeMethod('_call', array($callable, $result, $params));
		});
	}

	/**
	 * Attempts to apply a set of formatting rules from `$_rules` to a `$params` array, where each
	 * formatting rule is applied if the key of the rule in `$_rules` is present and not empty in
	 * `$params`.  Also performs sanity checking against `$params` to ensure that no value
	 * matching a rule is present unless the rule check passes.
	 *
	 * @param array $params An array of route parameters to which rules will be applied.
	 * @return array Returns the `$params` array with formatting rules applied to array values.
	 */
	public static function applyRules(&$params) {
		$result = array();
		$values = array();
		$rules = static::$_rules;

		if (!$params) {
			return false;
		}

		if (isset($params['controller']) && is_string($params['controller'])) {
			$controller = $params['controller'];

			if (strpos($controller, '.') !== false) {
				list($library, $controller) = explode('.', $controller);
				$controller = $library . '.' . Inflector::camelize($controller);
				$params += compact('library');
			} elseif (strpos($controller, '\\') === false) {
				$controller = Inflector::camelize($controller);

				if (isset($params['library'])) {
					$controller = "{$params['library']}.{$controller}";
				}
			}
			$values = compact('controller');
		}
		$values += $params;

		if (is_callable($rules)) {
			$rules = $rules($params);
		}
		foreach ($rules as $rule => $value) {
			if (!isset($values[$rule])) {
				continue;
			}
			foreach ($value as $k => $v) {
				if (is_callable($v)) {
					$result[$k] = $v($values);
					continue;
				}
				$match = preg_replace('/\{:\w+\}/', '@', $v);
				$match = preg_replace('/@/', '.+', preg_quote($match, '/'));
				if (preg_match('/' . $match . '/i', $values[$k])) {
					continue;
				}
				$result[$k] = String::insert($v, $values);
			}
		}
		return $result + $values;
	}

	/**
	 * Accepts parameters generated by the `Router` class in `Dispatcher::run()`, and produces a
	 * callable controller object. By default, this method uses the `'controller'` path lookup
	 * configuration in `Libraries::locate()` to return a callable object.
	 *
	 * @param object $request The instance of the `Request` class either passed into or generated by
	 *               `Dispatcher::run()`.
	 * @param array $params The parameter array generated by routing the request.
	 * @param array $options Not currently implemented.
	 * @return object Returns a callable object which the request will be routed to.
	 * @filter
	 */
	protected static function _callable($request, $params, $options) {
		$params = compact('request', 'params', 'options');

		return static::_filter(__FUNCTION__, $params, function($self, $params) {
			$options = array('request' => $params['request']) + $params['options'];
			$controller = $params['params']['controller'];

			try {
				return Libraries::instance('controllers', $controller, $options);
			} catch (ClassNotFoundException $e) {
				throw new DispatchException("Controller `{$controller}` not found.", null, $e);
			}
		});
	}

	/**
	 * Invokes the callable object returned by `_callable()`, and returns the results, usually a
	 * `Response` object instance.
	 *
	 * @see lithium\action
	 * @param object $callable Typically a closure or instance of `lithium\action\Controller`.
	 * @param object $request An instance of `lithium\action\Request`.
	 * @param array $params An array of parameters to pass to `$callable`, along with `$request`.
	 * @return mixed Returns the return value of `$callable`, usually an instance of
	 *         `lithium\action\Response`.
	 * @throws lithium\action\DispatchException Throws an exception if `$callable` is not a
	 *         `Closure`, or does not declare the PHP magic `__invoke()` method.
	 * @filter
	 */
	protected static function _call($callable, $request, $params) {
		$params = compact('callable', 'request', 'params');
		return static::_filter(__FUNCTION__, $params, function($self, $params) {
			if (is_callable($callable = $params['callable'])) {
				return $callable($params['request'], $params['params']);
			}
			throw new DispatchException('Result not callable.');
		});
	}
}

?>