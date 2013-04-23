<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2012, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\analysis\logger\adapter;

/**
 * The Syslog adapter facilitates logging messages to a `syslogd` backend. See the constructor for
 * information on configuring this adapter.
 *
 * @see lithium\analysis\logger\adapter\Syslog::__construct()
 */
class Syslog extends \lithium\core\Object {

	/**
	 * Flag indicating whether or not the connection to `syslogd` has been opened yet.
	 *
	 * @var boolean
	 */
	protected $_isConnected = false;

	/**
	 * Array that maps `Logger` message priority names to `syslog`-compatible priority constants.
	 *
	 * @var array
	 */
	protected $_priorities = array(
		'emergency' => LOG_EMERG,
		'alert'     => LOG_ALERT,
		'critical'  => LOG_CRIT,
		'error'     => LOG_ERR,
		'warning'   => LOG_WARNING,
		'notice'    => LOG_NOTICE,
		'info'      => LOG_INFO,
		'debug'     => LOG_DEBUG
	);

	/**
	 * Class constructor. Configures the `Syslog` adapter instance with the default settings. For
	 * more information on these settings, see the documentation for
	 * [the `openlog()` function](http://php.net/openlog).
	 *
	 * @param array $config Available configuration settings for this adapter:
	 *              - `'identity'` _string_: The identity string to be attached to each message in
	 *                the system log. This is usually a string that meaningfully identifies your
	 *                application. Defaults to `false`.
	 *              - `'options'` _integer_: The flags to use when opening the log. Defaults to
	 *                `LOG_ODELAY`.
	 *              - `'facility'` _integer_: A flag specifying the program to use to log the
	 *                messages. See the `openlog()` documentation for more information. Defaults to
	 *                `LOG_USER`.
	 */
	public function __construct(array $config = array()) {
		$defaults = array('identity' => false, 'options'  => LOG_ODELAY, 'facility' => LOG_USER);
		parent::__construct($config + $defaults);
	}

	/**
	 * Appends `$message` to the system log.
	 *
	 * @param string $priority The message priority string. Maps to a `syslogd` priority constant.
	 * @param string $message The message to write.
	 * @return closure Function returning boolean `true` on successful write, `false` otherwise.
	 */
	public function write($priority, $message) {
		$config = $this->_config;
		$_priorities = $this->_priorities;

		if (!$this->_isConnected) {
			closelog();
			openlog($config['identity'], $config['options'], $config['facility']);
			$this->_isConnected = true;
		}

		return function($self, $params) use ($_priorities) {
			$priority = $_priorities[$params['priority']];
			return syslog($priority, $params['message']);
		};
	}
}

?>