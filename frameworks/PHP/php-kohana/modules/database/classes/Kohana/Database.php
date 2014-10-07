<?php defined('SYSPATH') OR die('No direct script access.');
/**
 * Database connection wrapper/helper.
 *
 * You may get a database instance using `Database::instance('name')` where
 * name is the [config](database/config) group.
 *
 * This class provides connection instance management via Database Drivers, as
 * well as quoting, escaping and other related functions. Querys are done using
 * [Database_Query] and [Database_Query_Builder] objects, which can be easily
 * created using the [DB] helper class.
 *
 * @package    Kohana/Database
 * @category   Base
 * @author     Kohana Team
 * @copyright  (c) 2008-2012 Kohana Team
 * @license    http://kohanaphp.com/license
 */
abstract class Kohana_Database {

	// Query types
	const SELECT =  1;
	const INSERT =  2;
	const UPDATE =  3;
	const DELETE =  4;

	/**
	 * @var  string  default instance name
	 */
	public static $default = 'default';

	/**
	 * @var  array  Database instances
	 */
	public static $instances = array();

	/**
	 * Get a singleton Database instance. If configuration is not specified,
	 * it will be loaded from the database configuration file using the same
	 * group as the name.
	 *
	 *     // Load the default database
	 *     $db = Database::instance();
	 *
	 *     // Create a custom configured instance
	 *     $db = Database::instance('custom', $config);
	 *
	 * @param   string   $name    instance name
	 * @param   array    $config  configuration parameters
	 * @return  Database
	 */
	public static function instance($name = NULL, array $config = NULL)
	{
		if ($name === NULL)
		{
			// Use the default instance name
			$name = Database::$default;
		}

		if ( ! isset(Database::$instances[$name]))
		{
			if ($config === NULL)
			{
				// Load the configuration for this database
				$config = Kohana::$config->load('database')->$name;
			}

			if ( ! isset($config['type']))
			{
				throw new Kohana_Exception('Database type not defined in :name configuration',
					array(':name' => $name));
			}

			// Set the driver class name
			$driver = 'Database_'.ucfirst($config['type']);

			// Create the database connection instance
			$driver = new $driver($name, $config);

			// Store the database instance
			Database::$instances[$name] = $driver;
		}

		return Database::$instances[$name];
	}

	/**
	 * @var  string  the last query executed
	 */
	public $last_query;

	// Character that is used to quote identifiers
	protected $_identifier = '"';

	// Instance name
	protected $_instance;

	// Raw server connection
	protected $_connection;

	// Configuration array
	protected $_config;

	/**
	 * Stores the database configuration locally and name the instance.
	 *
	 * [!!] This method cannot be accessed directly, you must use [Database::instance].
	 *
	 * @return  void
	 */
	public function __construct($name, array $config)
	{
		// Set the instance name
		$this->_instance = $name;

		// Store the config locally
		$this->_config = $config;

		if (empty($this->_config['table_prefix']))
		{
			$this->_config['table_prefix'] = '';
		}
	}

	/**
	 * Disconnect from the database when the object is destroyed.
	 *
	 *     // Destroy the database instance
	 *     unset(Database::instances[(string) $db], $db);
	 *
	 * [!!] Calling `unset($db)` is not enough to destroy the database, as it
	 * will still be stored in `Database::$instances`.
	 *
	 * @return  void
	 */
	public function __destruct()
	{
		$this->disconnect();
	}

	/**
	 * Returns the database instance name.
	 *
	 *     echo (string) $db;
	 *
	 * @return  string
	 */
	public function __toString()
	{
		return $this->_instance;
	}

	/**
	 * Connect to the database. This is called automatically when the first
	 * query is executed.
	 *
	 *     $db->connect();
	 *
	 * @throws  Database_Exception
	 * @return  void
	 */
	abstract public function connect();

	/**
	 * Disconnect from the database. This is called automatically by [Database::__destruct].
	 * Clears the database instance from [Database::$instances].
	 *
	 *     $db->disconnect();
	 *
	 * @return  boolean
	 */
	public function disconnect()
	{
		unset(Database::$instances[$this->_instance]);

		return TRUE;
	}

	/**
	 * Set the connection character set. This is called automatically by [Database::connect].
	 *
	 *     $db->set_charset('utf8');
	 *
	 * @throws  Database_Exception
	 * @param   string   $charset  character set name
	 * @return  void
	 */
	abstract public function set_charset($charset);

	/**
	 * Perform an SQL query of the given type.
	 *
	 *     // Make a SELECT query and use objects for results
	 *     $db->query(Database::SELECT, 'SELECT * FROM groups', TRUE);
	 *
	 *     // Make a SELECT query and use "Model_User" for the results
	 *     $db->query(Database::SELECT, 'SELECT * FROM users LIMIT 1', 'Model_User');
	 *
	 * @param   integer  $type       Database::SELECT, Database::INSERT, etc
	 * @param   string   $sql        SQL query
	 * @param   mixed    $as_object  result object class string, TRUE for stdClass, FALSE for assoc array
	 * @param   array    $params     object construct parameters for result class
	 * @return  object   Database_Result for SELECT queries
	 * @return  array    list (insert id, row count) for INSERT queries
	 * @return  integer  number of affected rows for all other queries
	 */
	abstract public function query($type, $sql, $as_object = FALSE, array $params = NULL);

	/**
	 * Start a SQL transaction
	 *
	 *     // Start the transactions
	 *     $db->begin();
	 *
	 *     try {
	 *          DB::insert('users')->values($user1)...
	 *          DB::insert('users')->values($user2)...
	 *          // Insert successful commit the changes
	 *          $db->commit();
	 *     }
	 *     catch (Database_Exception $e)
	 *     {
	 *          // Insert failed. Rolling back changes...
	 *          $db->rollback();
	 *      }
	 *
	 * @param string $mode  transaction mode
	 * @return  boolean
	 */
	abstract public function begin($mode = NULL);

	/**
	 * Commit the current transaction
	 *
	 *     // Commit the database changes
	 *     $db->commit();
	 *
	 * @return  boolean
	 */
	abstract public function commit();

	/**
	 * Abort the current transaction
	 *
	 *     // Undo the changes
	 *     $db->rollback();
	 *
	 * @return  boolean
	 */
	abstract public function rollback();

	/**
	 * Count the number of records in a table.
	 *
	 *     // Get the total number of records in the "users" table
	 *     $count = $db->count_records('users');
	 *
	 * @param   mixed    $table  table name string or array(query, alias)
	 * @return  integer
	 */
	public function count_records($table)
	{
		// Quote the table name
		$table = $this->quote_table($table);

		return $this->query(Database::SELECT, 'SELECT COUNT(*) AS total_row_count FROM '.$table, FALSE)
			->get('total_row_count');
	}

	/**
	 * Returns a normalized array describing the SQL data type
	 *
	 *     $db->datatype('char');
	 *
	 * @param   string  $type  SQL data type
	 * @return  array
	 */
	public function datatype($type)
	{
		static $types = array
		(
			// SQL-92
			'bit'                           => array('type' => 'string', 'exact' => TRUE),
			'bit varying'                   => array('type' => 'string'),
			'char'                          => array('type' => 'string', 'exact' => TRUE),
			'char varying'                  => array('type' => 'string'),
			'character'                     => array('type' => 'string', 'exact' => TRUE),
			'character varying'             => array('type' => 'string'),
			'date'                          => array('type' => 'string'),
			'dec'                           => array('type' => 'float', 'exact' => TRUE),
			'decimal'                       => array('type' => 'float', 'exact' => TRUE),
			'double precision'              => array('type' => 'float'),
			'float'                         => array('type' => 'float'),
			'int'                           => array('type' => 'int', 'min' => '-2147483648', 'max' => '2147483647'),
			'integer'                       => array('type' => 'int', 'min' => '-2147483648', 'max' => '2147483647'),
			'interval'                      => array('type' => 'string'),
			'national char'                 => array('type' => 'string', 'exact' => TRUE),
			'national char varying'         => array('type' => 'string'),
			'national character'            => array('type' => 'string', 'exact' => TRUE),
			'national character varying'    => array('type' => 'string'),
			'nchar'                         => array('type' => 'string', 'exact' => TRUE),
			'nchar varying'                 => array('type' => 'string'),
			'numeric'                       => array('type' => 'float', 'exact' => TRUE),
			'real'                          => array('type' => 'float'),
			'smallint'                      => array('type' => 'int', 'min' => '-32768', 'max' => '32767'),
			'time'                          => array('type' => 'string'),
			'time with time zone'           => array('type' => 'string'),
			'timestamp'                     => array('type' => 'string'),
			'timestamp with time zone'      => array('type' => 'string'),
			'varchar'                       => array('type' => 'string'),

			// SQL:1999
			'binary large object'               => array('type' => 'string', 'binary' => TRUE),
			'blob'                              => array('type' => 'string', 'binary' => TRUE),
			'boolean'                           => array('type' => 'bool'),
			'char large object'                 => array('type' => 'string'),
			'character large object'            => array('type' => 'string'),
			'clob'                              => array('type' => 'string'),
			'national character large object'   => array('type' => 'string'),
			'nchar large object'                => array('type' => 'string'),
			'nclob'                             => array('type' => 'string'),
			'time without time zone'            => array('type' => 'string'),
			'timestamp without time zone'       => array('type' => 'string'),

			// SQL:2003
			'bigint'    => array('type' => 'int', 'min' => '-9223372036854775808', 'max' => '9223372036854775807'),

			// SQL:2008
			'binary'            => array('type' => 'string', 'binary' => TRUE, 'exact' => TRUE),
			'binary varying'    => array('type' => 'string', 'binary' => TRUE),
			'varbinary'         => array('type' => 'string', 'binary' => TRUE),
		);

		if (isset($types[$type]))
			return $types[$type];

		return array();
	}

	/**
	 * List all of the tables in the database. Optionally, a LIKE string can
	 * be used to search for specific tables.
	 *
	 *     // Get all tables in the current database
	 *     $tables = $db->list_tables();
	 *
	 *     // Get all user-related tables
	 *     $tables = $db->list_tables('user%');
	 *
	 * @param   string   $like  table to search for
	 * @return  array
	 */
	abstract public function list_tables($like = NULL);

	/**
	 * Lists all of the columns in a table. Optionally, a LIKE string can be
	 * used to search for specific fields.
	 *
	 *     // Get all columns from the "users" table
	 *     $columns = $db->list_columns('users');
	 *
	 *     // Get all name-related columns
	 *     $columns = $db->list_columns('users', '%name%');
	 *
	 *     // Get the columns from a table that doesn't use the table prefix
	 *     $columns = $db->list_columns('users', NULL, FALSE);
	 *
	 * @param   string  $table       table to get columns from
	 * @param   string  $like        column to search for
	 * @param   boolean $add_prefix  whether to add the table prefix automatically or not
	 * @return  array
	 */
	abstract public function list_columns($table, $like = NULL, $add_prefix = TRUE);

	/**
	 * Extracts the text between parentheses, if any.
	 *
	 *     // Returns: array('CHAR', '6')
	 *     list($type, $length) = $db->_parse_type('CHAR(6)');
	 *
	 * @param   string  $type
	 * @return  array   list containing the type and length, if any
	 */
	protected function _parse_type($type)
	{
		if (($open = strpos($type, '(')) === FALSE)
		{
			// No length specified
			return array($type, NULL);
		}

		// Closing parenthesis
		$close = strrpos($type, ')', $open);

		// Length without parentheses
		$length = substr($type, $open + 1, $close - 1 - $open);

		// Type without the length
		$type = substr($type, 0, $open).substr($type, $close + 1);

		return array($type, $length);
	}

	/**
	 * Return the table prefix defined in the current configuration.
	 *
	 *     $prefix = $db->table_prefix();
	 *
	 * @return  string
	 */
	public function table_prefix()
	{
		return $this->_config['table_prefix'];
	}

	/**
	 * Quote a value for an SQL query.
	 *
	 *     $db->quote(NULL);   // 'NULL'
	 *     $db->quote(10);     // 10
	 *     $db->quote('fred'); // 'fred'
	 *
	 * Objects passed to this function will be converted to strings.
	 * [Database_Expression] objects will be compiled.
	 * [Database_Query] objects will be compiled and converted to a sub-query.
	 * All other objects will be converted using the `__toString` method.
	 *
	 * @param   mixed   $value  any value to quote
	 * @return  string
	 * @uses    Database::escape
	 */
	public function quote($value)
	{
		if ($value === NULL)
		{
			return 'NULL';
		}
		elseif ($value === TRUE)
		{
			return "'1'";
		}
		elseif ($value === FALSE)
		{
			return "'0'";
		}
		elseif (is_object($value))
		{
			if ($value instanceof Database_Query)
			{
				// Create a sub-query
				return '('.$value->compile($this).')';
			}
			elseif ($value instanceof Database_Expression)
			{
				// Compile the expression
				return $value->compile($this);
			}
			else
			{
				// Convert the object to a string
				return $this->quote( (string) $value);
			}
		}
		elseif (is_array($value))
		{
			return '('.implode(', ', array_map(array($this, __FUNCTION__), $value)).')';
		}
		elseif (is_int($value))
		{
			return (int) $value;
		}
		elseif (is_float($value))
		{
			// Convert to non-locale aware float to prevent possible commas
			return sprintf('%F', $value);
		}

		return $this->escape($value);
	}

	/**
	 * Quote a database column name and add the table prefix if needed.
	 *
	 *     $column = $db->quote_column($column);
	 *
	 * You can also use SQL methods within identifiers.
	 *
	 *     $column = $db->quote_column(DB::expr('COUNT(`column`)'));
	 *
	 * Objects passed to this function will be converted to strings.
	 * [Database_Expression] objects will be compiled.
	 * [Database_Query] objects will be compiled and converted to a sub-query.
	 * All other objects will be converted using the `__toString` method.
	 *
	 * @param   mixed   $column  column name or array(column, alias)
	 * @return  string
	 * @uses    Database::quote_identifier
	 * @uses    Database::table_prefix
	 */
	public function quote_column($column)
	{
		// Identifiers are escaped by repeating them
		$escaped_identifier = $this->_identifier.$this->_identifier;

		if (is_array($column))
		{
			list($column, $alias) = $column;
			$alias = str_replace($this->_identifier, $escaped_identifier, $alias);
		}

		if ($column instanceof Database_Query)
		{
			// Create a sub-query
			$column = '('.$column->compile($this).')';
		}
		elseif ($column instanceof Database_Expression)
		{
			// Compile the expression
			$column = $column->compile($this);
		}
		else
		{
			// Convert to a string
			$column = (string) $column;

			$column = str_replace($this->_identifier, $escaped_identifier, $column);

			if ($column === '*')
			{
				return $column;
			}
			elseif (strpos($column, '.') !== FALSE)
			{
				$parts = explode('.', $column);

				if ($prefix = $this->table_prefix())
				{
					// Get the offset of the table name, 2nd-to-last part
					$offset = count($parts) - 2;

					// Add the table prefix to the table name
					$parts[$offset] = $prefix.$parts[$offset];
				}

				foreach ($parts as & $part)
				{
					if ($part !== '*')
					{
						// Quote each of the parts
						$part = $this->_identifier.$part.$this->_identifier;
					}
				}

				$column = implode('.', $parts);
			}
			else
			{
				$column = $this->_identifier.$column.$this->_identifier;
			}
		}

		if (isset($alias))
		{
			$column .= ' AS '.$this->_identifier.$alias.$this->_identifier;
		}

		return $column;
	}

	/**
	 * Quote a database table name and adds the table prefix if needed.
	 *
	 *     $table = $db->quote_table($table);
	 *
	 * Objects passed to this function will be converted to strings.
	 * [Database_Expression] objects will be compiled.
	 * [Database_Query] objects will be compiled and converted to a sub-query.
	 * All other objects will be converted using the `__toString` method.
	 *
	 * @param   mixed   $table  table name or array(table, alias)
	 * @return  string
	 * @uses    Database::quote_identifier
	 * @uses    Database::table_prefix
	 */
	public function quote_table($table)
	{
		// Identifiers are escaped by repeating them
		$escaped_identifier = $this->_identifier.$this->_identifier;

		if (is_array($table))
		{
			list($table, $alias) = $table;
			$alias = str_replace($this->_identifier, $escaped_identifier, $alias);
		}

		if ($table instanceof Database_Query)
		{
			// Create a sub-query
			$table = '('.$table->compile($this).')';
		}
		elseif ($table instanceof Database_Expression)
		{
			// Compile the expression
			$table = $table->compile($this);
		}
		else
		{
			// Convert to a string
			$table = (string) $table;

			$table = str_replace($this->_identifier, $escaped_identifier, $table);

			if (strpos($table, '.') !== FALSE)
			{
				$parts = explode('.', $table);

				if ($prefix = $this->table_prefix())
				{
					// Get the offset of the table name, last part
					$offset = count($parts) - 1;

					// Add the table prefix to the table name
					$parts[$offset] = $prefix.$parts[$offset];
				}

				foreach ($parts as & $part)
				{
					// Quote each of the parts
					$part = $this->_identifier.$part.$this->_identifier;
				}

				$table = implode('.', $parts);
			}
			else
			{
				// Add the table prefix
				$table = $this->_identifier.$this->table_prefix().$table.$this->_identifier;
			}
		}

		if (isset($alias))
		{
			// Attach table prefix to alias
			$table .= ' AS '.$this->_identifier.$this->table_prefix().$alias.$this->_identifier;
		}

		return $table;
	}

	/**
	 * Quote a database identifier
	 *
	 * Objects passed to this function will be converted to strings.
	 * [Database_Expression] objects will be compiled.
	 * [Database_Query] objects will be compiled and converted to a sub-query.
	 * All other objects will be converted using the `__toString` method.
	 *
	 * @param   mixed   $value  any identifier
	 * @return  string
	 */
	public function quote_identifier($value)
	{
		// Identifiers are escaped by repeating them
		$escaped_identifier = $this->_identifier.$this->_identifier;

		if (is_array($value))
		{
			list($value, $alias) = $value;
			$alias = str_replace($this->_identifier, $escaped_identifier, $alias);
		}

		if ($value instanceof Database_Query)
		{
			// Create a sub-query
			$value = '('.$value->compile($this).')';
		}
		elseif ($value instanceof Database_Expression)
		{
			// Compile the expression
			$value = $value->compile($this);
		}
		else
		{
			// Convert to a string
			$value = (string) $value;

			$value = str_replace($this->_identifier, $escaped_identifier, $value);

			if (strpos($value, '.') !== FALSE)
			{
				$parts = explode('.', $value);

				foreach ($parts as & $part)
				{
					// Quote each of the parts
					$part = $this->_identifier.$part.$this->_identifier;
				}

				$value = implode('.', $parts);
			}
			else
			{
				$value = $this->_identifier.$value.$this->_identifier;
			}
		}

		if (isset($alias))
		{
			$value .= ' AS '.$this->_identifier.$alias.$this->_identifier;
		}

		return $value;
	}

	/**
	 * Sanitize a string by escaping characters that could cause an SQL
	 * injection attack.
	 *
	 *     $value = $db->escape('any string');
	 *
	 * @param   string   $value  value to quote
	 * @return  string
	 */
	abstract public function escape($value);

} // End Database_Connection
