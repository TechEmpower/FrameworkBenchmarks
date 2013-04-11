<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\console;

/**
 * The `Request` class represents a console request and holds information about it's
 * environment as well as passed arguments.
 *
 * @see lithium\console\Dispatcher
 */
class Request extends \lithium\core\Object {

	/**
	 * The raw data passed from the command line
	 *
	 * @var array
	 */
	public $argv = array();

	/**
	 * Parameters parsed from arguments.
	 *
	 * @see lithium\console\Router
	 * @var array
	 */
	public $params = array(
		'command' => null, 'action' => 'run', 'args' => array()
	);

	/**
	 * Input (STDIN).
	 *
	 * @var resource
	 */
	public $input;

	/**
	 * Enviroment variables.
	 *
	 * @var array
	 */
	protected $_env = array();

	/**
	 * Holds the value of the current locale, set through the `locale()` method.
	 *
	 * @var string
	 */
	protected $_locale = null;

	/**
	 * Auto configuration
	 *
	 * @var array
	 */
	protected $_autoConfig = array('env' => 'merge');

	/**
	 * Class Constructor
	 *
	 * @param array $config
	 */
	public function __construct($config = array()) {
		$defaults = array('args' => array(), 'input' => null);
		$config += $defaults;
		parent::__construct($config);
	}

	/**
	 * Initialize request object, pulling request data from superglobals.
	 *
	 * Defines an artificial `'PLATFORM'` environment variable as `'CLI'` to
	 * allow checking for the SAPI in a normalized way. This is also for
	 * establishing consistency with this class' sister classes.
	 *
	 * @see lithium\action\Request::_init()
	 * @return void
	 */
	protected function _init() {
		$this->_env += (array) $_SERVER + (array) $_ENV;
		$this->_env['working'] = getcwd() ?: null;
		$argv = (array) $this->env('argv');
		$this->_env['script'] = array_shift($argv);
		$this->_env['PLATFORM'] = 'CLI';
		$this->argv += $argv + (array) $this->_config['args'];
		$this->input = $this->_config['input'];

		if (!is_resource($this->_config['input'])) {
			$this->input = fopen('php://stdin', 'r');
		}
		parent::_init();
	}

	/**
	 * Allows request parameters to be accessed as object properties, i.e. `$this->request->action`
	 * instead of `$this->request->params['action']`.
	 *
	 * @see lithium\action\Request::$params
	 * @param string $name The property name/parameter key to return.
	 * @return mixed Returns the value of `$params[$name]` if it is set, otherwise returns null.
	 */
	public function __get($name) {
		if (isset($this->params[$name])) {
			return $this->params[$name];
		}
	}

	public function __isset($name) {
		return isset($this->params[$name]);
	}

	/**
	 * Get the value of a command line argument at a given key
	 *
	 * @param integer $key
	 * @return mixed returns null if key does not exist or the value of the key in the args array
	 */
	public function args($key = 0) {
		if (!empty($this->args[$key])) {
			return $this->args[$key];
		}
		return null;
	}

	/**
	 * Get environment variables.
	 *
	 * @param string $key
	 * @return mixed Returns the environment key related to the `$key` argument. If `$key` is equal
	 * to null the result will be the entire environment array. If `$key` is set but not
	 * available, `null` will be returned.
	 */
	public function env($key = null) {
		if (!empty($this->_env[$key])) {
			return $this->_env[$key];
		}
		if ($key === null) {
			return $this->_env;
		}
		return null;
	}

	/**
	 * Moves params up a level. Sets command to action, action to passed[0], and so on.
	 *
	 * @param integer $num how many times to shift
	 * @return self
	 */
	public function shift($num = 1) {
		for ($i = $num; $i > 1; $i--) {
			$this->shift(--$i);
		}
		$this->params['command'] = $this->params['action'];
		if (isset($this->params['args'][0])) {
			$this->params['action'] = array_shift($this->params['args']);
		}
		return $this;
	}

	/**
	 * Reads a line from input.
	 *
	 * @return string
	 */
	public function input() {
		return fgets($this->input);
	}

	/**
	 * Sets or returns the current locale string. For more information, see
	 * "[Globalization](http://lithify.me/docs/manual/07_globalization)" in the manual.
	 *
	 * @param string $locale An optional locale string like `'en'`, `'en_US'` or `'de_DE'`. If
	 *               specified, will overwrite the existing locale.
	 * @return Returns the currently set locale string.
	 */
	public function locale($locale = null) {
		if ($locale) {
			$this->_locale = $locale;
		}
		return $this->_locale;
	}

	/**
	 * Return input
	 * Destructor. Closes input.
	 *
	 * @return void
	 */
	public function __destruct() {
		if ($this->input) {
			fclose($this->input);
		}
	}
}

?>