<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 *
 */

namespace lithium\data\source\database\adapter;

use PDOException;
use lithium\core\ConfigException;

/**
 * Sqlite database driver
 *
 * @todo fix encoding methods to use class query methods instead of sqlite3 natives
 */
class Sqlite3 extends \lithium\data\source\Database {

	/**
	 * Pair of opening and closing quote characters used for quoting identifiers in queries.
	 *
	 * @link http://www.sqlite.org/lang_keywords.html
	 * @var array
	 */
	protected $_quotes = array('"', '"');

	/**
	 * Sqlite column type definitions.
	 *
	 * @var array
	 */
	protected $_columns = array(
		'primary_key' => array('name' => 'primary key autoincrement'),
		'string' => array('name' => 'varchar', 'length' => 255),
		'text' => array('name' => 'text'),
		'integer' => array('name' => 'integer', 'formatter' => 'intval'),
		'float' => array('name' => 'float', 'formatter' => 'floatval'),
		'datetime' => array('name' => 'datetime', 'format' => 'Y-m-d H:i:s', 'formatter' => 'date'),
		'timestamp' => array(
			'name' => 'timestamp', 'format' => 'Y-m-d H:i:s', 'formatter' => 'date'
		),
		'time' => array('name' => 'time', 'format' => 'H:i:s', 'formatter' => 'date'),
		'date' => array('name' => 'date', 'format' => 'Y-m-d', 'formatter' => 'date'),
		'binary' => array('name' => 'blob'),
		'boolean' => array('name' => 'boolean')
	);

	/**
	 * Holds commonly regular expressions used in this class.
	 *
	 * @see lithium\data\source\database\adapter\Sqlite3::describe()
	 * @see lithium\data\source\database\adapter\Sqlite3::_column()
	 * @var array
	 */
	protected $_regex = array(
		'column' => '(?P<type>[^(]+)(?:\((?P<length>[^)]+)\))?'
	);

	/**
	 * Constructs the Sqlite adapter
	 *
	 * @see lithium\data\source\Database::__construct()
	 * @see lithium\data\Source::__construct()
	 * @see lithium\data\Connections::add()
	 * @param array $config Configuration options for this class. For additional configuration,
	 *        see `lithium\data\source\Database` and `lithium\data\Source`. Available options
	 *        defined by this class:
	 *        - `'database'` _string_: database name. Defaults to none
	 *        - `'flags'` _integer_: Optional flags used to determine how to open the SQLite
	 *          database. By default, open uses SQLITE3_OPEN_READWRITE | SQLITE3_OPEN_CREATE.
	 *        - `'key'` _string_: An optional encryption key used when encrypting and decrypting
	 *          an SQLite database.
	 *
	 * Typically, these parameters are set in `Connections::add()`, when adding the adapter to the
	 * list of active connections.
	 */
	public function __construct(array $config = array()) {
		$defaults = array('database' => ':memory:', 'encoding' => null);
		parent::__construct($config + $defaults);
	}

	/**
	 * Check for required PHP extension, or supported database feature.
	 *
	 * @param string $feature Test for support for a specific feature, i.e. `'transactions'`.
	 * @return boolean Returns `true` if the particular feature (or if Sqlite) support is enabled,
	 *         otherwise `false`.
	 */
	public static function enabled($feature = null) {
		if (!$feature) {
			return extension_loaded('pdo_sqlite');
		}
		$features = array(
			'arrays' => false,
			'transactions' => false,
			'booleans' => true,
			'relationships' => true
		);
		return isset($features[$feature]) ? $features[$feature] : null;
	}

	/**
	 * Connects to the database using options provided to the class constructor.
	 *
	 * @return boolean True if the database could be connected, else false
	 */
	public function connect() {
		if (!$this->_config['database']) {
			throw new ConfigException('No Database configured');
		}

		if (empty($this->_config['dsn'])) {
			$this->_config['dsn'] = sprintf("sqlite:%s", $this->_config['database']);
		}

		return parent::connect();
	}

	/**
	 * Disconnects the adapter from the database.
	 *
	 * @return boolean True on success, else false.
	 */
	public function disconnect() {
		if ($this->_isConnected) {
			unset($this->connection);
			$this->_isConnected = false;
		}
		return true;
	}

	/**
	 * Returns the list of tables in the currently-connected database.
	 *
	 * @param string $model The fully-name-spaced class name of the model object making the request.
	 * @return array Returns an array of objects to which models can connect.
	 * @filter This method can be filtered.
	 */
	public function sources($model = null) {
		$config = $this->_config;

		return $this->_filter(__METHOD__, compact('model'), function($self, $params) use ($config) {
			$sql = "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name;";
			$result = $self->invokeMethod('_execute', array($sql));
			$sources = array();

			while ($data = $result->next()) {
				$sources[] = reset($data);
			}
			return $sources;
		});
	}

	/**
	 * Gets the column schema for a given Sqlite3 table.
	 *
	 * A column type may not always be available, i.e. when during creation of
	 * the column no type was declared. Those columns are internally treated
	 * by SQLite3 as having a `NONE` affinity. The final schema will contain no
	 * information about type and length of such columns (both values will be
	 * `null`).
	 *
	 * @param mixed $entity Specifies the table name for which the schema should be returned, or
	 *        the class name of the model object requesting the schema, in which case the model
	 *        class will be queried for the correct table name.
	 * @param array $fields Any schema data pre-defined by the model.
	 * @param array $meta
	 * @return array Returns an associative array describing the given table's schema, where the
	 *         array keys are the available fields, and the values are arrays describing each
	 *         field, containing the following keys:
	 *         - `'type'`: The field type name
	 * @filter This method can be filtered.
	 */
	public function describe($entity, $fields = array(), array $meta = array()) {
		$params = compact('entity', 'meta', 'fields');
		$regex = $this->_regex;
		return $this->_filter(__METHOD__, $params, function($self, $params) use ($regex) {
			extract($params);

			if ($fields) {
				return $self->invokeMethod('_instance', array('schema', compact('fields')));
			}
			$name = $self->invokeMethod('_entityName', array($entity, array('quoted' => true)));
			$columns = $self->read("PRAGMA table_info({$name})", array('return' => 'array'));
			$fields = array();
			foreach ($columns as $column) {
				preg_match("/{$regex['column']}/", $column['type'], $matches);

				$fields[$column['name']] = array(
					'type' => isset($matches['type']) ? $matches['type'] : null,
					'length' => isset($matches['length']) ? $matches['length'] : null,
					'null' => $column['notnull'] == 1,
					'default' => $column['dflt_value']
				);
			}
			return $self->invokeMethod('_instance', array('schema', compact('fields')));
		});
	}

	/**
	 * Gets the last auto-generated ID from the query that inserted a new record.
	 *
	 * @param object $query The `Query` object associated with the query which generated
	 * @return mixed Returns the last inserted ID key for an auto-increment column or a column
	 *         bound to a sequence.
	 */
	protected function _insertId($query) {
		return $this->connection->lastInsertId();
	}

	/**
	 * Gets or sets the encoding for the connection.
	 *
	 * @param string $encoding If setting the encoding, this is the name of the encoding to set,
	 *               i.e. `'utf8'` or `'UTF-8'` (both formats are valid).
	 * @return mixed If setting the encoding; returns `true` on success, or `false` on
	 *         failure. When getting, returns the encoding as a string.
	 */
	public function encoding($encoding = null) {
		$encodingMap = array('UTF-8' => 'utf8');

		if (!$encoding) {
			$query = $this->connection->query('PRAGMA encoding');
			$encoding = $query->fetchColumn();
			return ($key = array_search($encoding, $encodingMap)) ? $key : $encoding;
		}
		$encoding = isset($encodingMap[$encoding]) ? $encodingMap[$encoding] : $encoding;

		try {
			$this->connection->exec("PRAGMA encoding = \"{$encoding}\"");
			return true;
		} catch (PDOException $e) {
			return false;
		}
	}

	/**
	 * Retrieves database error message and error code.
	 *
	 * @return array
	 */
	public function error() {
		if ($error = $this->connection->errorInfo()) {
			return array($error[1], $error[2]);
		}
	}

	/**
	 * Execute a given query.
	 *
	 * @see lithium\data\source\Database::renderCommand()
	 * @param string $sql The sql string to execute
	 * @param array $options No available options.
	 * @return resource
	 * @filter
	 */
	protected function _execute($sql, array $options = array()) {
		$conn = $this->connection;
		$params = compact('sql', 'options');
		return $this->_filter(__METHOD__, $params, function($self, $params) use ($conn) {
			$sql = $params['sql'];
			$options = $params['options'];
			try {
				$resource = $conn->query($sql);
			} catch(PDOException $e) {
				$self->invokeMethod('_error', array($sql));
			};
			return $self->invokeMethod('_instance', array('result', compact('resource')));
		});
	}

	/**
	 * Converts database-layer column types to basic types.
	 *
	 * @param string $real Real database-layer column type (i.e. "varchar(255)")
	 * @return string Abstract column type (i.e. "string")
	 */
	protected function _column($real) {
		if (is_array($real)) {
			return $real['type'] . (isset($real['length']) ? "({$real['length']})" : '');
		}

		if (!preg_match("/{$this->_regex['column']}/", $real, $column)) {
			return $real;
		}
		$column = array_intersect_key($column, array('type' => null, 'length' => null));

		if (isset($column['length']) && $column['length']) {
			$length = explode(',', $column['length']) + array(null, null);
			$column['length'] = $length[0] ? intval($length[0]) : null;
			$length[1] ? $column['precision'] = intval($length[1]) : null;
		}

		switch (true) {
			case in_array($column['type'], array('date', 'time', 'datetime', 'timestamp')):
				return $column;
			case ($column['type'] === 'tinyint' && $column['length'] == '1'):
			case ($column['type'] === 'boolean'):
				return array('type' => 'boolean');
			break;
			case (strpos($column['type'], 'int') !== false):
				$column['type'] = 'integer';
			break;
			case (strpos($column['type'], 'char') !== false || $column['type'] === 'tinytext'):
				$column['type'] = 'string';
			break;
			case (strpos($column['type'], 'text') !== false):
				$column['type'] = 'text';
			break;
			case (strpos($column['type'], 'blob') !== false || $column['type'] === 'binary'):
				$column['type'] = 'binary';
			break;
			case preg_match('/float|double|decimal/', $column['type']):
				$column['type'] = 'float';
			break;
			default:
				$column['type'] = 'text';
			break;
		}
		return $column;
	}
}

?>