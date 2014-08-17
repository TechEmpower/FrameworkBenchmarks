<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\console;

use lithium\core\Libraries;
use lithium\core\Environment;
use UnexpectedValueException;

/**
 * The `Dispatcher` is the outermost layer of the framework, responsible for both receiving the
 * initial console request and returning back a response at the end of the request's life cycle.
 *
 * The console dispatcher is responsible for accepting requests from scripts called from the command
 * line, and executing the appropriate `Command` class(es). The `run()` method accepts an instance
 * of `lithium\console\Request`, which encapsulates the console environment and any command-line
 * parameters passed to the script. `Dispatcher` then invokes `lithium\console\Router` to determine
 * the correct `Command` class to invoke, and which method should be called.
 */
class Dispatcher extends \lithium\core\StaticObject {

	/**
	 * Fully-namespaced router class reference.
	 *
	 * Class must implement a `parse()` method, which must return an array with (at a minimum)
	 * 'command' and 'action' keys.
	 *
	 * @see lithium\console\Router::parse()
	 * @var array
	 */
	protected static $_classes = array(
		'request' => 'lithium\console\Request',
		'router' => 'lithium\console\Router'
	);

	/**
	 * Contains pre-process format strings for changing Dispatcher's behavior based on 'rules'.
	 *
	 * Each key in the array represents a 'rule'; if a key that matches the rule is present (and
	 * not empty) in a route, (i.e. the result of `lithium\console\Router::parse()`) then the rule's
	 * value will be applied to the route before it is dispatched.  When applying a rule, any array
	 * elements array elements of the flag which are present in the route will be modified using a
	 * `lithium\util\String::insert()`-formatted string.
	 *
	 * @see lithium\console\Dispatcher::config()
	 * @see lithium\util\String::insert()
	 * @var array
	 */
	protected static $_rules = array(
		'command' => array(array('lithium\util\Inflector', 'camelize')),
		'action' => array(array('lithium\util\Inflector', 'camelize', array(false)))
	);

	/**
	 * Used to set configuration parameters for the Dispatcher.
	 *
	 * @param array $config Optional configuration params.
	 * @return array If no parameters are passed, returns an associative array with the
	 *         current configuration, otherwise returns null.
	 */
	public static function config($config = array()) {
		if (!$config) {
			return array('rules' => static::$_rules);
		}
		foreach ($config as $key => $val) {
			if (isset(static::${'_' . $key})) {
				static::${'_' . $key} = $val + static::${'_' . $key};
			}
		}
	}

	/**
	 * Dispatches a request based on a request object (an instance of `lithium\console\Request`).
	 *
	 *  If `$request` is `null`, a new request object is instantiated based on the value of the
	 * `'request'` key in the `$_classes` array.
	 *
	 * @param object $request An instance of a request object with console request information.  If
	 *        `null`, an instance will be created.
	 * @param array $options
	 * @return object The command action result which is an instance of `lithium\console\Response`.
	 */
	public static function run($request = null, $options = array()) {
		$defaults = array('request' => array());
		$options += $defaults;
		$classes = static::$_classes;
		$params = compact('request', 'options');

		return static::_filter(__FUNCTION__, $params, function($self, $params) use ($classes) {
			$request = $params['request'];
			$options = $params['options'];
			$router = $classes['router'];
			$request = $request ?: new $classes['request']($options['request']);
			$request->params = $router::parse($request);
			$params = $self::applyRules($request->params);
			Environment::set($request);
			try {
				$callable = $self::invokeMethod('_callable', array($request, $params, $options));
				return $self::invokeMethod('_call', array($callable, $request, $params));
			} catch (UnexpectedValueException $e) {
				return (object) array('status' => $e->getMessage() . "\n");
			}
		});
	}

	/**
	 * Determines which command to use for current request.
	 *
	 * @param object $request An instance of a `Request` object.
	 * @param array $params Request params that can be accessed inside the filter.
	 * @param array $options
	 * @return class lithium\console\Command Returns the instantiated command object.
	 */
	protected static function _callable($request, $params, $options) {
		$params = compact('request', 'params', 'options');
		return static::_filter(__FUNCTION__, $params, function($self, $params) {
			$request = $params['request'];
			$params = $params['params'];
			$name = $params['command'];

			if (!$name) {
				$request->params['args'][0] = $name;
				$name = 'lithium\console\command\Help';
			}
			if (class_exists($class = Libraries::locate('command', $name))) {
				return new $class(compact('request'));
			}
			throw new UnexpectedValueException("Command `{$name}` not found.");
		});
	}

	/**
	 * Attempts to apply a set of formatting rules from `$_rules` to a `$params` array.
	 *
	 * Each formatting rule is applied if the key of the rule in `$_rules` is present and not empty
	 * in `$params`.  Also performs sanity checking against `$params` to ensure that no value
	 * matching a rule is present unless the rule check passes.
	 *
	 * @param array $params An array of route parameters to which rules will be applied.
	 * @return array Returns the `$params` array with formatting rules applied to array values.
	 */
	public static function applyRules($params) {
		$result = array();

		if (!$params) {
			return false;
		}

		foreach (static::$_rules as $name => $rules) {
			foreach ($rules as $rule) {
				if (!empty($params[$name]) && isset($rule[0])) {
					$options = array_merge(
						array($params[$name]), isset($rule[2]) ? (array) $rule[2] : array()
					);
					$result[$name] = call_user_func_array(array($rule[0], $rule[1]), $options);
				}
			}
		}
		return $result + array_diff_key($params, $result);
	}

	/**
	 * Calls a given command with the appropriate action.
	 *
	 * This method is responsible for calling a `$callable` command and returning its result.
	 *
	 * @param string $callable The callable command.
	 * @param string $request The associated `Request` object.
	 * @param string $params Additional params that should be passed along.
	 * @return mixed Returns the result of the called action, typically `true` or `false`.
	 */
	protected static function _call($callable, $request, $params) {
		$params = compact('callable', 'request', 'params');
		return static::_filter(__FUNCTION__, $params, function($self, $params) {
			if (is_callable($callable = $params['callable'])) {
				$request = $params['request'];
				$params = $params['params'];

				if (!method_exists($callable, $params['action'])) {
					array_unshift($params['args'], $request->params['action']);
					$params['action'] = 'run';
				}
				$isHelp = (
					!empty($params['help']) || !empty($params['h'])
					|| !method_exists($callable, $params['action'])
				);
				if ($isHelp) {
					$params['action'] = '_help';
				}
				return $callable($params['action'], $params['args']);
			}
			throw new UnexpectedValueException("Callable `{$callable}` is actually not callable.");
		});
	}
}

?>