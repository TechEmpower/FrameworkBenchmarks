<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\security;

use lithium\core\ConfigException;

/**
 * The `Auth` class provides a common interface to authenticate user credentials from different
 * sources against different storage backends in a uniform way. As with most other adapter-driven
 * classes in the framework, `Auth` allows you to specify one or more named configurations,
 * including an adapter, which can be referenced by name in your application.
 *
 * `Auth` is responsible for managing session state for each configuration, and exposes a set of
 * methods which adapters can implement: `set()`, `check()` and `clear()`. You can read more about
 * each method below. Beyond these methods, `Auth` makes very few assumptions about how your
 * application authenticates users. Each adapter accepts a set of credentials, and returns an array
 * of user information on success, and `false` on failure. On successful authentication attempts,
 * the data returned from the credential check is written to the session, which is automatically
 * accessed on subsequent checks (though manual re-checking can be forced on a per-instance basis).
 *
 * To be secure by default (and if you don't override it), a `password` field is never stored in
 * the session adapter. This prevents a possible password hash to be leaked in a cookie (for
 * example). You can also be very specific on what you want to store in the session:
 *
 * {{{
 * Auth::config(array(
 *   'default' => array(
 *      'session' => array(
 *        'persist' => array('username', 'email')
 *      )
 *   )
 * ));
 * }}}
 *
 * You can also pass an optional `persist` param to the `check` method to override this default.
 *
 * For additional information on configuring and working with `Auth`, see the `Form` adapter.
 *
 * @see lithium\security\auth\adapter\Form
 */
class Auth extends \lithium\core\Adaptable {

	/**
	 * Stores configurations for various authentication adapters.
	 *
	 * @var object `Collection` of authentication configurations.
	 */
	protected static $_configurations = array();

	/**
	 * Libraries::locate() compatible path to adapters for this class.
	 *
	 * @see lithium\core\Libraries::locate()
	 * @var string Dot-delimited path.
	 */
	protected static $_adapters = 'adapter.security.auth';

	/**
	 * Dynamic class dependencies.
	 *
	 * @var array Associative array of class names & their namespaces.
	 */
	protected static $_classes = array(
		'session' => 'lithium\storage\Session'
	);

	/**
	 * Called when an adapter configuration is first accessed, this method sets the default
	 * configuration for session handling. While each configuration can use its own session class
	 * and options, this method initializes them to the default dependencies written into the class.
	 * For the session key name, the default value is set to the name of the configuration.
	 *
	 * @param string $name The name of the adapter configuration being accessed.
	 * @param array $config The user-specified configuration.
	 * @return array Returns an array that merges the user-specified configuration with the
	 *         generated default values.
	 */
	protected static function _initConfig($name, $config) {
		$defaults = array('session' => array(
			'key' => $name,
			'class' => static::$_classes['session'],
			'options' => array(),
			'persist' => array()
		));
		$config = parent::_initConfig($name, $config) + $defaults;
		$config['session'] += $defaults['session'];
		return $config;
	}

	/**
	 * Performs an authentication check against the specified configuration, and writes the
	 * resulting user information to the session such that credentials are not required for
	 * subsequent authentication checks, and user information is returned directly from the session.
	 *
	 * @param string $name The name of the `Auth` configuration/adapter to check against.
	 * @param mixed $credentials A container for the authentication credentials used in this check.
	 *              This will vary by adapter, but generally will be an object or array containing
	 *              a user name and password. In the case of the `Form` adapter, it contains a
	 *              `Request` object containing `POST` data with user login information.
	 * @param array $options Additional options used when performing the authentication check. The
	 *              options available will vary by adapter, please consult the documentation for the
	 *              `check()` method of the adapter you intend to use. The global options for this
	 *              method are:
	 *              - `'checkSession'` _boolean_: By default, the session store configured for the
	 *                adapter will always be queried first, to see if an authentication check has
	 *                already been performed during the current user session. If yes, then the
	 *                session data will be returned. By setting `'checkSession'` to `false`,
	 *                session checks are bypassed and the credentials provided are always checked
	 *                against the adapter directly.
	 *              - `'writeSession'` _boolean_: Upon a successful credentials check, the returned
	 *                user information is, by default, written to the session. Set this to `false`
	 *                to disable session writing for this authentication check.
	 *              - `'persist'` _array_: A list of fields that should be stored in the session.
	 * @return array After a successful credential check against the adapter (or a successful
	 *         lookup against the current session), returns an array of user information from the
	 *         storage backend used by the configured adapter.
	 * @filter
	 */
	public static function check($name, $credentials = null, array $options = array()) {
		$config = static::config($name);
		$defaults = array(
			'checkSession' => true,
			'writeSession' => true,
			'persist' => $config['session']['persist'] ?: static::_config('persist')
		);

		$options += $defaults;
		$params = compact('name', 'credentials', 'options');

		return static::_filter(__FUNCTION__, $params, function($self, $params) {
			extract($params);
			$config = $self::invokeMethod('_config', array($name));
			$persist = $options['persist'];

			if ($config === null) {
				throw new ConfigException("Configuration `{$name}` has not been defined.");
			}
			$session = $config['session'];

			if ($options['checkSession']) {
				if ($data = $session['class']::read($session['key'], $session['options'])) {
					return $data;
				}
			}

			if (($credentials) && $data = $self::adapter($name)->check($credentials, $options)) {
				if ($options['persist'] && is_array($data)) {
					$data = array_intersect_key($data, array_fill_keys($options['persist'], true));
				} elseif (is_array($data)) {
					unset($data['password']);
				}
				return ($options['writeSession']) ? $self::set($name, $data) : $data;
			}
			return false;
		});
	}

	/**
	 * Manually authenticate a user with the given set of data. Rather than checking a user's
	 * credentials, this method allows you to manually specify a user for whom you'd like to
	 * initialize an authenticated session.
	 *
	 * By default, before writing the data to the session, the `set()` method of the named
	 * configuration's adapter receives the data to be written, and has an opportunity to modify
	 * or reject it.
	 *
	 * @param string $name The name of the adapter configuration to.
	 * @param array $data The user data to be written to the session.
	 * @param array $options Any additional session-writing options. These may override any options
	 *              set by the default session configuration for `$name`.
	 * @return array Returns the array of data written to the session, or `false` if the adapter
	 *         rejects the data.
	 * @filter
	 */
	public static function set($name, $data, array $options = array()) {
		$params = compact('name', 'data', 'options');

		return static::_filter(__FUNCTION__, $params, function($self, $params) {
			extract($params);
			$config = $self::invokeMethod('_config', array($name));
			$session = $config['session'];

			if ($data = $self::adapter($name)->set($data, $options)) {
				$session['class']::write($session['key'], $data, $options + $session['options']);
				return $data;
			}
			return false;
		});
	}

	/**
	 * Removes session information for the given configuration, and allows the configuration's
	 * adapter to perform any associated cleanup tasks.
	 *
	 * @param string $name The name of the `Auth` configuration to clear the login information for.
	 *               Calls the `clear()` method of the given configuration's adapter, and removes
	 *               the information in the session key used by this configuration.
	 * @param array $options Additional options used when clearing the authenticated session. See
	 *              each adapter's `clear()` method for all available options. Global options:
	 *              - `'clearSession'` _boolean_: If `true` (the default), session data for the
	 *                specified configuration is removed, otherwise it is retained.
	 * @return void
	 * @filter
	 */
	public static function clear($name, array $options = array()) {
		$defaults = array('clearSession' => true);
		$options += $defaults;

		return static::_filter(__FUNCTION__, compact('name', 'options'), function($self, $params) {
			extract($params);
			$config = $self::invokeMethod('_config', array($name));
			$session = $config['session'];

			if ($options['clearSession']) {
				$session['class']::delete($session['key'], $session['options']);
			}
			$self::adapter($name)->clear($options);
		});
	}
}

?>