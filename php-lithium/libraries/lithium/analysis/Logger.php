<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2012, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\analysis;

use UnexpectedValueException;

/**
 * The `Logger` class provides a consistent, application-wide interface for configuring and writing
 * log messages. As with other subclasses of `Adaptable`, `Logger` can be configured with a series
 * of named configurations, each containing a log adapter to write to. `Logger` exposes a single
 * method, `write()`, which can write to one or more log adapters.
 *
 * When configuring adapters, you may specify one or more priorities for each, using the
 * `'priority'` key. This key can be a single priority level (string), or an array of multiple
 * levels. When a log message is written, all adapters that are configured to accept the priority
 * level with which the message was written will receive the message.
 *
 * {{{
 * Logger::config(array(
 * 	'default' => array('adapter' => 'Syslog'),
 * 	'badnews' => array(
 * 		'adapter' => 'File',
 * 		'priority' => array('emergency', 'alert', 'critical', 'error')
 * 	)
 * ));
 * }}}
 *
 * In the above configuration, all messages will be written to the system log (`syslogd`), but only
 * messages with the priority `error` or higher will be logged to a file. Messages can then be
 * written to the log(s) using the `write()` method:
 *
 * {{{ Logger::write('alert', 'This is an alert-level message that will be logged in 2 places'); }}}
 *
 * Messages can also be written using the log priority as a method name:
 *
 * {{{ Logger::alert('This is an alert-level message that will be logged in 2 places'); }}}
 *
 * This works identically to the above. The message priority levels which `Logger` supports are as
 * follows: `emergency`, `alert`, `critical`, `error`, `warning`, `notice`, `info` and `debug`.
 * Attempting to use any other priority level will raise an exception. See the list of available
 * adapters for more information on what adapters are available, and how to configure them.
 *
 * @see lithium\analysis\logger\adapter
 */
class Logger extends \lithium\core\Adaptable {

	/**
	 * Stores configurations for cache adapters.
	 *
	 * @var object `Collection` of logger configurations.
	 */
	protected static $_configurations = array();

	/**
	 * Libraries::locate() compatible path to adapters for this class.
	 *
	 * @see lithium\core\Libraries::locate()
	 * @var string Dot-delimited path.
	 */
	protected static $_adapters = 'adapter.analysis.logger';

	/**
	 * An array of valid message priorities.
	 *
	 * @var array
	 */
	protected static $_priorities = array(
		'emergency' => 0,
		'alert'     => 1,
		'critical'  => 2,
		'error'     => 3,
		'warning'   => 4,
		'notice'    => 5,
		'info'      => 6,
		'debug'     => 7
	);

	/**
	 * Writes a message to one or more log adapters, where the adapters that are written to are the
	 * ones that respond to the given priority level.
	 *
	 * @param string $priority The priority of the log message to be written.
	 * @param string $message The message to be written.
	 * @param array $options An array of adapter-specific options that may be passed when writing
	 *              log messages. Some options are also handled by `Logger` itself:
	 *              - `'name'` _string_: This option can be specified if you wish to write to a
	 *                specific adapter configuration, instead of writing to the adapter(s) that
	 *                respond to the given priority.
	 * @return boolean Returns `true` if all log writes succeeded, or `false` if _any or all_ writes
	 *         failed.
	 * @throws UnexpectedValueException If the value of `$priority` is not a defined priority value,
	 *         an `UnexpectedValueException` will be thrown.
	 * @filter
	 */
	public static function write($priority, $message, array $options = array()) {
		$defaults = array('name' => null);
		$options += $defaults;
		$result = true;

		if (isset(self::$_configurations[$options['name']])) {
			$name = $options['name'];
			$methods = array($name => static::adapter($name)->write($priority, $message, $options));
		} elseif (!isset(static::$_priorities[$priority])) {
			$message = "Attempted to write log message with invalid priority `{$priority}`.";
			throw new UnexpectedValueException($message);
		} else {
			$methods = static::_configsByPriority($priority, $message, $options);
		}

		foreach ($methods as $name => $method) {
			$params = compact('priority', 'message', 'options');
			$config = static::_config($name);
			$result &= static::_filter(__FUNCTION__, $params, $method, $config['filters']);
		}
		return $methods ? $result : false;
	}

	/**
	 * Acts as a proxy for the `write()` method, allowing log message priority names to be called as
	 * methods, i.e.:
	 * {{{
	 * Logger::emergency('Something bad happened.');
	 * // This is equivalent to Logger::write('emergency', 'Something bad happened')
	 * }}}
	 *
	 * @param string $priority The name of the method called on the `Logger` class. This should map
	 *               to a log type.
	 * @param array $params An array of parameters passed in the method.
	 * @return boolean Returns `true` or `false`, depending on the success of the `write()` method.
	 */
	public static function __callStatic($priority, $params) {
		$params += array(null, array());
		return static::write($priority, $params[0], $params[1]);
	}

	/**
	 * Custom check to determine if our given magic methods can be responded to.
	 *
	 * @param  string  $method     Method name.
	 * @param  bool    $internal   Interal call or not.
	 * @return bool
	 */
	public static function respondsTo($method, $internal = false) {
		return isset(static::$_priorities[$method]) || parent::respondsTo($method, $internal);
	}

	/**
	 * This method is called automatically to initialize the default configuration of a log adapter,
	 * such that the adapter defaults to accepting log messages of any priority (i.e. the
	 * `'priority'` key is set to `true`).
	 *
	 * @param string $name The name of the logger configuration.
	 * @param array $config The logger configuration as specified in application code.
	 * @return array Returns an array of configuration data, merged with default values.
	 */
	protected static function _initConfig($name, $config) {
		$defaults = array('priority' => true);
		return parent::_initConfig($name, $config) + $defaults;
	}

	/**
	 * Gets the names of the adapter configurations that respond to a specific priority. The list
	 * of adapter configurations returned will be used to write a message with the given priority.
	 *
	 * @param string $priority The priority level of a message to be written.
	 * @param string $message The message to write to the adapter.
	 * @param array $options Adapter-specific options.
	 * @return array Returns an array of names of configurations which are set up to respond to the
	 *         message priority specified in `$priority`, or configured to respond to _all_ message
	 *        priorities.
	 */
	protected static function _configsByPriority($priority, $message, array $options = array()) {
		$configs = array();
		$key = 'priority';

		foreach (array_keys(static::$_configurations) as $name) {
			$config = static::config($name);
			$nameMatch = ($config[$key] === true || $config[$key] === $priority);
			$arrayMatch = (is_array($config[$key]) && in_array($priority, $config[$key]));

			if ($nameMatch || $arrayMatch) {
				$method = static::adapter($name)->write($priority, $message, $options);
				$method ? $configs[$name] = $method : null;
			}
		}
		return $configs;
	}
}

?>