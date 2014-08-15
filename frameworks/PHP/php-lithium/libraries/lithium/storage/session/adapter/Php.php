<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\storage\session\adapter;

use lithium\util\Set;
use RuntimeException;
use lithium\core\ConfigException;

/**
 * A minimal adapter to interface with native PHP sessions.
 *
 * This adapter provides basic support for `write`, `read` and `delete`
 * session handling, as well as allowing these three methods to be filtered as
 * per the Lithium filtering system.
 *
 */
class Php extends \lithium\core\Object {

	/**
	 * Default ini settings for this session adapter.
	 *
	 * @var array Keys are session ini settings, with the `session.` namespace.
	 */
	protected $_defaults = array(
		'session.cookie_lifetime' => '0',
		'session.cookie_httponly' => true
	);

	/**
	 * Class constructor.
	 *
	 * Takes care of setting appropriate configurations for this object.
	 *
	 * @param array $config Unified constructor configuration parameters. You can set
	 *        the `session.*` PHP ini settings here as key/value pairs.
	 */
	public function __construct(array $config = array()) {
		if (empty($config['session.name'])) {
			$config['session.name'] = basename(LITHIUM_APP_PATH);
		}
		parent::__construct($config + $this->_defaults);
	}

	/**
	 * Initialization of the session.
	 *
	 * @todo Split up into an _initialize() and a _start().
	 * @return void
	 */
	protected function _init() {
		if (static::isStarted()) {
			return true;
		}
		$config = $this->_config;
		unset($config['adapter'], $config['strategies'], $config['filters'], $config['init']);

		foreach ($config as $key => $value) {
			if (strpos($key, 'session.') === false) {
				continue;
			}
			if (ini_set($key, $value) === false) {
				throw new ConfigException("Could not initialize the session.");
			}
		}
	}

	/**
	 * Starts the session.
	 *
	 * @return boolean True if session successfully started (or has already been started),
	 *         false otherwise.
	 */
	protected static function _start() {
		if (session_id()) {
			return true;
		}
		if (!isset($_SESSION)) {
			session_cache_limiter('nocache');
		}
		return session_start();
	}

	/**
	 * Obtain the status of the session.
	 *
	 * @return boolean True if $_SESSION is accessible and if a '_timestamp' key
	 *         has been set, false otherwise.
	 */
	public static function isStarted() {
		return (boolean) session_id();
	}

	/**
	 * Sets or obtains the session ID.
	 *
	 * @param string $key Optional. If specified, sets the session ID to the value of `$key`.
	 * @return mixed Session ID, or `null` if the session has not been started.
	 */
	public static function key($key = null) {
		if ($key) {
			return session_id($key);
		}
		return session_id() ?: null;
	}

	/**
	 * Checks if a value has been set in the session.
	 *
	 * @param string $key Key of the entry to be checked.
	 * @param array $options Options array. Not used for this adapter method.
	 * @return closure Function returning boolean `true` if the key exists, `false` otherwise.
	 */
	public static function check($key, array $options = array()) {
		if (!static::isStarted() && !static::_start()) {
			throw new RuntimeException("Could not start session.");
		}
		return function($self, $params) {
			return Set::check($_SESSION, $params['key']);
		};
	}

	/**
	 * Read a value from the session.
	 *
	 * @param null|string $key Key of the entry to be read. If no key is passed, all
	 *        current session data is returned.
	 * @param array $options Options array. Not used for this adapter method.
	 * @return closure Function returning data in the session if successful, `false` otherwise.
	 */
	public static function read($key = null, array $options = array()) {
		if (!static::isStarted() && !static::_start()) {
			throw new RuntimeException("Could not start session.");
		}
		return function($self, $params) {
			$key = $params['key'];

			if (!$key) {
				return $_SESSION;
			}
			if (strpos($key, '.') === false) {
				return isset($_SESSION[$key]) ? $_SESSION[$key] : null;
			}
			$filter  = function($keys, $data) use (&$filter) {
				$key = array_shift($keys);
				if (isset($data[$key])) {
					return (empty($keys)) ? $data[$key] : $filter($keys, $data[$key]);
				}
			};
			return $filter(explode('.', $key), $_SESSION);
		};
	}

	/**
	 * Write a value to the session.
	 *
	 * @param string $key Key of the item to be stored.
	 * @param mixed $value The value to be stored.
	 * @param array $options Options array. Not used for this adapter method.
	 * @return closure Function returning boolean `true` on successful write, `false` otherwise.
	 */
	public static function write($key, $value, array $options = array()) {
		if (!static::isStarted() && !static::_start()) {
			throw new RuntimeException("Could not start session.");
		}
		$class = __CLASS__;

		return function($self, $params) use ($class) {
			return $class::overwrite(
				$_SESSION, Set::insert($_SESSION, $params['key'], $params['value'])
			);
		};
	}

	/**
	 * Delete value from the session
	 *
	 * @param string $key The key to be deleted
	 * @param array $options Options array. Not used for this adapter method.
	 * @return closure Function returning boolean `true` if the key no longer exists
	 *         in the session, `false` otherwise
	 */
	public static function delete($key, array $options = array()) {
		if (!static::isStarted() && !static::_start()) {
			throw new RuntimeException("Could not start session.");
		}
		$class = __CLASS__;

		return function($self, $params) use ($class) {
			$key = $params['key'];
			$class::overwrite($_SESSION, Set::remove($_SESSION, $key));
			return !Set::check($_SESSION, $key);
		};
	}

	/**
	 * Clears all keys from the session.
	 *
	 * @param array $options Options array. Not used fro this adapter method.
	 * @return closure Function returning boolean `true` on successful clear, `false` otherwise.
	 */
	public function clear(array $options = array()) {
		if (!static::isStarted() && !static::_start()) {
			throw new RuntimeException("Could not start session.");
		}

		return function($self, $params) {
			return session_destroy();
		};
	}

	/**
	 * Determines if PHP sessions are enabled.
	 *
	 * @return boolean True if enabled (that is, if session_id() returns a value), false otherwise.
	 */
	public static function enabled() {
		return (boolean) session_id();
	}

	/**
	 * Overwrites session keys and values.
	 *
	 * @param array $old Reference to the array that needs to be overwritten. Will usually
	 *        be `$_SESSION`.
	 * @param array $new The data that should overwrite the keys/values in `$old`.
	 * @return boolean Always `true`
	 */
	public static function overwrite(&$old, $new) {
		if (!empty($old)) {
			foreach ($old as $key => $value) {
				if (!isset($new[$key])) {
					unset($old[$key]);
				}
			}
		}
		foreach ($new as $key => $value) {
			$old[$key] = $value;
		}
		return true;
	}
}

?>