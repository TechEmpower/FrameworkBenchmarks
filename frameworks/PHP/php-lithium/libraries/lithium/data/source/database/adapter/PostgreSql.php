<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright	  Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license		  http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\data\source\database\adapter;

use PDO;
use PDOException;

/**
 * Extends the `Database` class to implement the necessary SQL-formatting and resultset-fetching
 * features for working with PostgreSQL databases.
 *
 * For more information on configuring the database connection, see the `__construct()` method.
 *
 * @see lithium\data\source\database\adapter\PostgreSql::__construct()
 */
class PostgreSql extends \lithium\data\source\Database {

	/**
	 * PostgreSQL column type definitions.
	 *
	 * @var array
	 */
	protected $_columns = array(
		'primary_key' => array('name' => 'SERIAL not null'),
		'string' => array('name' => 'varchar', 'length' => 255),
		'text' => array('name' => 'text'),
		'integer' => array('name' => 'integer', 'formatter' => 'intval'),
		'float' => array('name' => 'float', 'formatter' => 'floatval'),
		'datetime' => array(
			'name' => 'timestamp', 'format' => 'Y-m-d H:i:s', 'formatter' => 'date'
		),
		'timestamp' => array(
			'name' => 'timestamp', 'format' => 'Y-m-d H:i:s', 'formatter' => 'date'
		),
		'time' => array('name' => 'time', 'format' => 'H:i:s', 'formatter' => 'date'),
		'date' => array('name' => 'date', 'format' => 'Y-m-d', 'formatter' => 'date'),
		'binary' => array('name' => 'bytea'),
		'boolean' => array('name' => 'boolean'),
		'number' => array('name' => 'numeric'),
		'inet' => array('name' => 'inet')
	);

	/**
	 * Pair of opening and closing quote characters used for quoting identifiers in queries.
	 *
	 * @var array
	 */
	protected $_quotes = array('"', '"');

	/**
	 * PostgreSQL-specific value denoting whether or not table aliases should be used in DELETE and
	 * UPDATE queries.
	 *
	 * @var boolean
	 */
	protected $_useAlias = true;

	/**
	 * Constructs the PostgreSQL adapter and sets the default port to 5432.
	 *
	 * @see lithium\data\source\Database::__construct()
	 * @see lithium\data\Source::__construct()
	 * @see lithium\data\Connections::add()
	 * @param array $config Configuration options for this class. For additional configuration,
	 *        see `lithium\data\source\Database` and `lithium\data\Source`. Available options
	 *        defined by this class:
	 *        - `'database'`: The name of the database to connect to. Defaults to 'lithium'.
	 *        - `'host'`: The IP or machine name where PostgreSQL is running, followed by a colon,
	 *        followed by a port number or socket. Defaults to `'localhost:5432'`.
	 *        - `'persistent'`: If a persistent connection (if available) should be made.
	 *        Defaults to true.
	 *        - `'schema'`: The name of the database schema to use. Defaults to 'public'
	 *
	 * Typically, these parameters are set in `Connections::add()`, when adding the adapter to the
	 * list of active connections.
	 */
	public function __construct(array $config = array()) {
		$defaults = array(
			'host' => 'localhost:5432',
			'encoding' => null,
			'schema' => 'public',
			'timezone' => null
		);
		parent::__construct($config + $defaults);
	}

	/**
	 * Check for required PHP extension, or supported database feature.
	 *
	 * @param string $feature Test for support for a specific feature, i.e. `"transactions"` or
	 *        `"arrays"`.
	 * @return boolean Returns `true` if the particular feature (or if PostgreSQL) support is
	 *         enabled, otherwise `false`.
	 */
	public static function enabled($feature = null) {
		if (!$feature) {
			return extension_loaded('pdo_pgsql');
		}
		$features = array(
			'arrays' => false,
			'transactions' => true,
			'booleans' => true,
			'relationships' => true
		);
		return isset($features[$feature]) ? $features[$feature] : null;
	}

	/**
	 * Connects to the database using the options provided to the class constructor.
	 *
	 * @return boolean Returns `true` if a database connection could be established, otherwise
	 *         `false`.
	 */
	public function connect() {
		if (!$this->_config['dsn']) {
			$host = $this->_config['host'];
			list($host, $port) = explode(':', $host) + array(1 => "5432");
			$dsn = "pgsql:host=%s;port=%s;dbname=%s";
			$this->_config['dsn'] = sprintf($dsn, $host, $port, $this->_config['database']);
		}

		if (!parent::connect()) {
			return false;
		}

		if ($this->_config['schema']) {
			$this->search_path($this->_config['schema']);
		}

		if ($this->_config['timezone']) {
			$this->timezone($this->_config['timezone']);
		}
		return true;
	}

	/**
	 * Returns the list of tables in the currently-connected database.
	 *
	 * @param string $model The fully-name-spaced class name of the model object making the request.
	 * @return array Returns an array of sources to which models can connect.
	 * @filter This method can be filtered.
	 */
	public function sources($model = null) {
		$_config = $this->_config;
		$params = compact('model');

		return $this->_filter(__METHOD__, $params, function($self, $params) use ($_config) {
			$schema = $self->connection->quote($_config['schema']);

			$sql = "SELECT table_name as name FROM INFORMATION_SCHEMA.tables";
			$sql .= " WHERE table_schema = {$schema}";

			if (!$result = $self->invokeMethod('_execute', array($sql))) {
				return null;
			}
			$sources = array();

			while ($data = $result->next()) {
				$sources[] = array_shift($data);
			}
			return $sources;
		});
	}

	/**
	 * Gets the column schema for a given PostgreSQL table.
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
		$schema = $this->_config['schema'];
		$params = compact('entity', 'meta', 'fields', 'schema');
		return $this->_filter(__METHOD__, $params, function($self, $params) {
			extract($params);

			if ($fields) {
				return $self->invokeMethod('_instance', array('schema', compact('fields')));
			}
			$name = $self->connection->quote($self->invokeMethod('_entityName', array($entity)));
			$schema = $self->connection->quote($schema);

			$sql = "SELECT DISTINCT table_schema AS schema, column_name AS field, data_type AS type,
					is_nullable AS null, column_default AS default, ordinal_position AS position,
					character_maximum_length AS char_length, character_octet_length AS oct_length
					FROM information_schema.columns
					WHERE table_name = {$name} AND table_schema = {$schema} ORDER BY position";

			$columns = $self->connection->query($sql)->fetchAll(PDO::FETCH_ASSOC);

			$fields = array();

			foreach ($columns as $column) {
				$match = $self->invokeMethod('_column', array($column['type']));

				if (preg_match('/nextval\([\'"]?([\w.]+)/', $column['default'])) {
					$default = null;
				} else {
					$default = $column['default'];
				}
				$fields[$column['field']] = $match + array(
					'null'	   => ($column['null'] == 'YES' ? true : false),
					'default'  => $default
				);
				if ($fields[$column['field']]['type'] == 'string') {
					$fields[$column['field']]['length'] = $column['char_length'];
				}
			}
			return $self->invokeMethod('_instance', array('schema', compact('fields')));
		});
	}

	/**
	 * Gets or sets the search path for the connection
	 * @param $search_path
	 * @return mixed If setting the search_path; returns ture on success, else false
	 *         When getting, returns the search_path
	 */
	public function search_path($search_path) {
		if (empty($search_path)) {
			$query = $this->connection->query('SHOW search_path');
			$search_path = $query->fetchColumn(1);
			return explode(",", $search_path);
		}
		try{
			$this->connection->exec("SET search_path TO ${search_path}");
			return true;
		} catch (PDOException $e) {
			return false;
		}
	}

	/**
	 * Gets or sets the time zone for the connection
	 * @param $timezone
	 * @return mixed If setting the time zone; returns true on success, else false
	 *         When getting, returns the time zone
	 */
	public function timezone($timezone = null) {
		if (empty($timezone)) {
			$query = $this->connection->query('SHOW TIME ZONE');
			return $query->fetchColumn();
		}
		try {
			$this->connection->exec("SET TIME ZONE '{$timezone}'");
			return true;
		} catch (PDOException $e) {
			return false;
		}
	}

	/**
	 * Gets or sets the encoding for the connection.
	 *
	 * @param $encoding
	 * @return mixed If setting the encoding; returns true on success, else false.
	 *         When getting, returns the encoding.
	 */
	public function encoding($encoding = null) {
		$encodingMap = array('UTF-8' => 'UTF8');

		if (empty($encoding)) {
			$query = $this->connection->query("SHOW client_encoding");
			$encoding = $query->fetchColumn();
			return ($key = array_search($encoding, $encodingMap)) ? $key : $encoding;
		}
		$encoding = isset($encodingMap[$encoding]) ? $encodingMap[$encoding] : $encoding;
		try {
			$this->connection->exec("SET NAMES '{$encoding}'");
			return true;
		} catch (PDOException $e) {
			return false;
		}
	}

	/**
	 * Converts a given value into the proper type based on a given schema definition.
	 *
	 * @see lithium\data\source\Database::schema()
	 * @param mixed $value The value to be converted. Arrays will be recursively converted.
	 * @param array $schema Formatted array from `lithium\data\source\Database::schema()`
	 * @return mixed Value with converted type.
	 */
	public function value($value, array $schema = array()) {
		if (($result = parent::value($value, $schema)) !== null) {
			return $result;
		}
		return $this->connection->quote((string) $value);
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
		return null;
	}

	public function alias($alias, $context) {
		if ($context->type() == 'update' || $context->type() == 'delete') {
			return;
		}
		return parent::alias($alias, $context);
	}

	/**
	 * @todo Eventually, this will need to rewrite aliases for DELETE and UPDATE queries, same with
	 *       order().
	 * @param string $conditions
	 * @param string $context
	 * @param array $options
	 * @return void
	 */
	public function conditions($conditions, $context, array $options = array()) {
		return parent::conditions($conditions, $context, $options);
	}

	/**
	 * Execute a given query.
	 *
	 * @see lithium\data\source\Database::renderCommand()
	 * @param string $sql The sql string to execute
	 * @param array $options Available options:
	 * @return resource Returns the result resource handle if the query is successful.
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
	 * Gets the last auto-generated ID from the query that inserted a new record.
	 *
	 * @param object $query The `Query` object associated with the query which generated
	 * @return mixed Returns the last inserted ID key for an auto-increment column or a column
	 *         bound to a sequence.
	 */
	protected function _insertId($query) {
		$model = $query->model();
		$field = $model::key();
		$source = $model::meta('source');
		$sequence = "{$source}_{$field}_seq";
		$id = $this->connection->lastInsertId($sequence);
		return ($id && $id !== '0') ? $id : null;
	}

	/**
	 * Converts database-layer column types to basic types.
	 *
	 * @param string $real Real database-layer column type (i.e. `"varchar(255)"`)
	 * @return array Column type (i.e. "string") plus 'length' when appropriate.
	 */
	protected function _column($real) {
		if (is_array($real)) {
			return $real['type'] . (isset($real['length']) ? "({$real['length']})" : '');
		}

		if (!preg_match('/(?P<type>\w+)(?:\((?P<length>[\d,]+)\))?/', $real, $column)) {
			return $real;
		}
		$column = array_intersect_key($column, array('type' => null, 'length' => null));

		if (isset($column['length']) && $column['length']) {
			$length = explode(',', $column['length']) + array(null, null);
			$column['length'] = $length[0] ? intval($length[0]) : null;
			$length[1] ? $column['precision'] = intval($length[1]) : null;
		}

		switch (true) {
			case in_array($column['type'], array('date', 'time', 'datetime')):
				return $column;
			case ($column['type'] == 'timestamp'):
				$column['type'] = 'datetime';
			break;
			case ($column['type'] == 'tinyint' && $column['length'] == '1'):
			case ($column['type'] == 'boolean'):
				return array('type' => 'boolean');
			break;
			case (strpos($column['type'], 'int') !== false):
				$column['type'] = 'integer';
			break;
			case (strpos($column['type'], 'char') !== false || $column['type'] == 'tinytext'):
				$column['type'] = 'string';
			break;
			case (strpos($column['type'], 'text') !== false):
				$column['type'] = 'text';
			break;
			case (strpos($column['type'], 'blob') !== false || $column['type'] == 'binary'):
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

	protected function _toNativeBoolean($value) {
		return $this->connection->quote($value ? 't' : 'f');
	}
}

?>