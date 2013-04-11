<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\data\source;

use lithium\util\String;
use lithium\data\model\Query;

/**
 * Http class to access data sources using `lithium\net\http\Service`.
 */
class Http extends \lithium\data\Source {

	/**
	 * Service connection
	 *
	 * @var object lithium\net\http\Service
	 */
	public $connection = null;

	/**
	 * The set of array keys which will be auto-populated in the object's protected properties from
	 * constructor parameters.
	 *
	 * @var array
	 */
	protected $_autoConfig = array('classes' => 'merge', 'methods' => 'merge');

	/**
	 * Fully-namespaced class references
	 *
	 * @var array
	 */
	protected $_classes = array(
		'schema'  => 'lithium\data\Schema',
		'service' => 'lithium\net\http\Service',
		'relationship' => 'lithium\data\model\Relationship'
	);

	/**
	 * Is Connected?
	 *
	 * @var boolean
	 */
	protected $_isConnected = false;

	/**
	 * List of methods and their corresponding HTTP method and path.
	 *
	 * @var array
	 */
	protected $_methods = array(
		'create' => array('method' => 'post', 'path' => "/{:source}"),
		'read'   => array('method' => 'get', 'path' => "/{:source}"),
		'update' => array('method' => 'put', 'path' => "/{:source}/{:id}"),
		'delete' => array('method' => 'delete', 'path' => "/{:source}/{:id}")
	);

	/**
	 * Constructor
	 *
	 * @param array $config
	 */
	public function __construct(array $config = array()) {
		$defaults = array(
			'adapter'    => null,
			'persistent' => false,
			'scheme'     => 'http',
			'host'       => 'localhost',
			'version'    => '1.1',
			'auth'       => null,
			'login'      => '',
			'password'   => '',
			'port'       => 80,
			'timeout'    => 30,
			'encoding'   => 'UTF-8',
			'methods'    => array()
		);
		$config = $config + $defaults;
		$config['username'] = $config['login'];
		parent::__construct($config);
	}

	protected function _init() {
		$config = $this->_config;
		unset($config['type']);
		$this->connection = $this->_instance('service', $config);
		parent::_init();
	}

	/**
	 * Pass properties to service connection
	 *
	 * @param string $property
	 * @return mixed
	 */
	public function __get($property) {
		return $this->connection->{$property};
	}

	/**
	 * Pass methods to service connection. Path and method are determined from Http::$_method. If
	 * not set, a GET request with the $method as the path will be used.
	 *
	 * @see lithium\data\source\Http::$_method
	 * @param string $method
	 * @param array $params
	 * @return mixed
	 * @filter
	 */
	public function __call($method, $params) {
		if (!isset($this->_methods[$method])) {
			if (method_exists($this->connection, $method)) {
				return $this->connection->invokeMethod($method, $params);
			}
			$this->_methods[$method] = array('path' => "/{$method}");
		}
		$params += array(array(), array());

		if (!is_object($params[0])) {
			$config = (array) $params[0];

			if (count($config) === count($config, COUNT_RECURSIVE)) {
				$config = array('data' => $config);
			}
			$params[0] = new Query($this->_methods[$method] + $config);
		}
		$params[0] = new Query($params[0]->export($this) + $this->_methods[$method]);

		return $this->_filter(__CLASS__ . "::" . $method, $params, function($self, $params) {
			list($query, $options) = $params;
			return $self->send($query, $options);
		});
	}

	/**
	 * Custom check to determine if our given magic methods can be responded to.
	 *
	 * @param  string  $method     Method name.
	 * @param  bool    $internal   Interal call or not.
	 * @return bool
	 */
	public function respondsTo($method, $internal = false) {
		return isset($this->_methods[$method]) || parent::respondsTo($method, $internal);
	}

	/**
	 * Method to send to a specific resource.
	 *
	 * @param array $query a query object
	 * @param array $options array.
	 * @return result
	 */
	public function send($query = null, array $options = array()) {
		$query = !is_object($query) ? new Query((array) $query) : $query;
		$method = $query->method() ?: "get";
		$path = $query->path();
		$data = $query->data();
		$insert = (array) $options + $data + $query->export($this);

		if (preg_match_all('/\{:(\w+)\}/', $path, $matches)) {
			$keys = array_flip($matches[1]);
			$data = array_diff_key($data,  array_flip($matches[1]));
		}
		$path = String::insert($path, $insert, array('clean' => true));
		$data += (array) $query->conditions() + array('limit' => $query->limit());
		return $this->connection->{$method}($path, $data, (array) $options);
	}

	/**
	 * Fake the connection since service is called for every method.
	 *
	 * @return boolean
	 */
	public function connect() {
		if (!$this->_isConnected) {
			$this->_isConnected = true;
		}
		return $this->_isConnected;
	}

	/**
	 * Disconnect from socket.
	 *
	 * @return boolean
	 */
	public function disconnect() {
		if ($this->_isConnected && $this->connection !== null) {
			$this->_isConnected = false;
		}
		return !$this->_isConnected;
	}

	/**
	 * Returns available data sources (typically a list of REST resources collections).
	 *
	 * @param object $class
	 * @return array
	 */
	public function sources($class = null) {
		return array();
	}

	/**
	 * Describe data source.
	 *
	 * @param string $entity
	 * @param array $fields
	 * @param array $meta
	 * @return array - returns an empty array
	 */
	public function describe($entity, $fields = array(), array $meta = array()) {
		return $this->_instance('schema', compact('fields', 'meta'));
	}

	/**
	 * Create function used to POST.
	 *
	 * @param object $query
	 * @param array $options
	 * @return void
	 * @filter
	 */
	public function create($query, array $options = array()) {
		$query = !is_object($query) ? new Query() : $query;
		$query->method() ?: $query->method("post");
		$query->path() ?: $query->path("/{:source}");
		return $this->_filter(__METHOD__, array($query, $options), function($self, $params) {
			list($query, $options) = $params;
			return $self->send($query, $options);
		});
	}

	/**
	 * Read used by model to GET.
	 *
	 * @param object $query
	 * @param array $options
	 * @return string
	 * @filter
	 */
	public function read($query, array $options = array()) {
		$query = !is_object($query) ? new Query() : $query;
		$query->method() ?: $query->method("get");
		$query->path() ?: $query->path("/{:source}");
		return $this->_filter(__METHOD__, array($query, $options), function($self, $params) {
			list($query, $options) = $params;
			return $self->send($query, $options);
		});
	}

	/**
	 * Update used by model to PUT.
	 *
	 * @param object $query
	 * @param array $options
	 * @return string
	 * @filter
	 */
	public function update($query, array $options = array()) {
		$query = !is_object($query) ? new Query() : $query;
		$query->method() ?: $query->method("put");
		$query->path() ?: $query->path("/{:source}/{:id}");
		return $this->_filter(__METHOD__, array($query, $options), function($self, $params) {
			list($query, $options) = $params;
			return $self->send($query, $options);
		});
	}

	/**
	 * Used by model to DELETE.
	 *
	 * @param object $query
	 * @param array $options
	 * @return string
	 * @filter
	 */
	public function delete($query, array $options = array()) {
		$query = !is_object($query) ? new Query() : $query;
		$query->method() ?: $query->method("delete");
		$query->path() ?: $query->path("/{:source}/{:id}");
		return $this->_filter(__METHOD__, array($query, $options), function($self, $params) {
			list($query, $options) = $params;
			return $self->send($query, $options);
		});

	}

	/**
	 * Defines or modifies the default settings of a relationship between two models.
	 *
	 * @param string $class
	 * @param string $type
	 * @param string $name
	 * @param array $options
	 * @return array Returns an array containing the configuration for a model relationship.
	 */
	public function relationship($class, $type, $name, array $options = array()) {
		if (isset($this->_classes['relationship'])) {
			return $this->_instance('relationship', compact('type', 'name') + $options);
		}
		return null;
	}
}

?>