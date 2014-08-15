<?php
/**
 * MySQLi database connection.
 *
 * @package    Fuel/Database
 * @category   Drivers
 * @author     Kohana Team
 * @copyright  (c) 2008-2009 Kohana Team
 * @license    http://kohanaphp.com/license
 */

namespace Fuel\Core;



class Database_MySQLi_Connection extends \Database_Connection
{
	/**
	 * @var  \MySQLi  Raw server connection
	 */
	protected $_connection;

	/**
	 * @var  array  Database in use by each connection
	 */
	protected static $_current_databases = array();

	/**
	 * @var  bool  Use SET NAMES to set the character set
	 */
	protected static $_set_names;

	/**
	 * @var  string  Identifier for this connection within the PHP driver
	 */
	protected $_connection_id;

	/**
	 * @var  string  MySQL uses a backtick for identifiers
	 */
	protected $_identifier = '`';

	/**
	 * @var  bool  Allows transactions
	 */
	protected $_in_transaction = false;

	/**
	 * @var  string  Which kind of DB is used
	 */
	public $_db_type = 'mysql';

	public function connect()
	{
		if ($this->_connection)
		{
			return;
		}

		if (static::$_set_names === null)
		{
			// Determine if we can use mysqli_set_charset(), which is only
			// available on PHP 5.2.3+ when compiled against MySQL 5.0+
			static::$_set_names = ! function_exists('mysqli_set_charset');
		}

		// Extract the connection parameters, adding required variables
		extract($this->_config['connection'] + array(
			'database'   => '',
			'hostname'   => '',
			'port'       => '',
			'socket'     => '',
			'username'   => '',
			'password'   => '',
			'persistent' => false,
			'compress'	 => true,
		));

		// Prevent this information from showing up in traces
		unset($this->_config['connection']['username'], $this->_config['connection']['password']);

		try
		{
			if ($socket != '')
			{
				$port   = null;
			}
			elseif ($port != '')
			{
				$socket = null;
			}
			else
			{
				$socket = null;
				$port   = null;
			}
			if ($persistent)
			{
				// Create a persistent connection
				if ($compress)
				{
					$mysqli = mysqli_init();
					$mysqli->real_connect('p:'.$hostname, $username, $password, $database, $port, $socket, MYSQLI_CLIENT_COMPRESS);

					$this->_connection = $mysqli;
				}
				else
				{
					$this->_connection = new \MySQLi('p:'.$hostname, $username, $password, $database, $port, $socket);
				}
			}
			else
			{
				// Create a connection and force it to be a new link
				if ($compress)
				{
					$mysqli = mysqli_init();
					$mysqli->real_connect($hostname, $username, $password, $database, $port, $socket, MYSQLI_CLIENT_COMPRESS);

					$this->_connection = $mysqli;
				}
				else
				{
					$this->_connection = new \MySQLi($hostname, $username, $password, $database, $port, $socket);
				}
			}
			if ($this->_connection->error)
			{
				// Unable to connect, select database, etc
				throw new \Database_Exception($this->_connection->error, $this->_connection->errno);
			}
		}
		catch (\ErrorException $e)
		{
			// No connection exists
			$this->_connection = null;

			throw new \Database_Exception('No MySQLi Connection', 0);
		}

		// \xFF is a better delimiter, but the PHP driver uses underscore
		$this->_connection_id = sha1($hostname.'_'.$username.'_'.$password);

		if ( ! empty($this->_config['charset']))
		{
			// Set the character set
			$this->set_charset($this->_config['charset']);
		}

		static::$_current_databases[$this->_connection_id] = $database;
	}

	/**
	 * Select the database
	 *
	 * @param   string  Database
	 * @return  void
	 */
	protected function _select_db($database)
	{
		if ($this->_config['connection']['database'] !== static::$_current_databases[$this->_connection_id])
		{
			if ($this->_connection->select_db($database) !== true)
			{
				// Unable to select database
				throw new \Database_Exception($this->_connection->error, $this->_connection->errno);
			}
		}

		static::$_current_databases[$this->_connection_id] = $database;
	}

	public function disconnect()
	{
		try
		{
			// Database is assumed disconnected
			$status = true;

			if ($this->_connection instanceof \MySQLi)
			{
				$status = $this->_connection->close();
			}
		}
		catch (\Exception $e)
		{
			// Database is probably not disconnected
			$status = ! ($this->_connection instanceof \MySQLi);
		}

		return $status;
	}

	public function set_charset($charset)
	{
		// Make sure the database is connected
		$this->_connection or $this->connect();
		$status = $this->_connection->set_charset($charset);

		if ($status === false)
		{
			throw new \Database_Exception($this->_connection->error, $this->_connection->errno);
		}
	}

	public function query($type, $sql, $as_object)
	{
		// Make sure the database is connected
		if ($this->_connection)
		{
			// Make sure the connection is still alive
			if ( ! $this->_connection->ping())
			{
				throw new \Database_Exception($this->_connection->error.' [ '.$sql.' ]', $this->_connection->errno);
			}
		}
		else
		{
			$this->connect();
		}

		if ( ! empty($this->_config['profiling']))
		{
			// Benchmark this query for the current instance
			$benchmark = \Profiler::start("Database ({$this->_instance})", $sql);
		}

		if ( ! empty($this->_config['connection']['persistent']) and $this->_config['connection']['database'] !== static::$_current_databases[$this->_connection_id])
		{
			// Select database on persistent connections
			$this->_select_db($this->_config['connection']['database']);
		}

		// Execute the query
		if (($result = $this->_connection->query($sql)) === false)
		{
			if (isset($benchmark))
			{
				// This benchmark is worthless
				\Profiler::delete($benchmark);
			}

			throw new \Database_Exception($this->_connection->error.' [ '.$sql.' ]', $this->_connection->errno);
		}

		// check for multiresults, we don't support those at the moment
		while($this->_connection->more_results() and $this->_connection->next_result())
		{
			if ($more_result = $this->_connection->use_result())
			{
				throw new \Database_Exception('The MySQLi driver does not support multiple resultsets', 0);
			}
		}

		if (isset($benchmark))
		{
			\Profiler::stop($benchmark);
		}

		// Set the last query
		$this->last_query = $sql;

		if ($type === \DB::SELECT)
		{
			// Return an iterator of results
			return new \Database_MySQLi_Result($result, $sql, $as_object);
		}
		elseif ($type === \DB::INSERT)
		{
			// Return a list of insert id and rows created
			return array(
				$this->_connection->insert_id,
				$this->_connection->affected_rows,
			);
		}
		else
		{
			// Return the number of rows affected
			return $this->_connection->affected_rows;
		}
	}

	public function datatype($type)
	{
		static $types = array
		(
			'blob'                      => array('type' => 'string', 'binary' => true, 'character_maximum_length' => '65535'),
			'bool'                      => array('type' => 'bool'),
			'bigint unsigned'           => array('type' => 'int', 'min' => '0', 'max' => '18446744073709551615'),
			'datetime'                  => array('type' => 'string'),
			'decimal unsigned'          => array('type' => 'float', 'exact' => true, 'min' => '0'),
			'double'                    => array('type' => 'float'),
			'double precision unsigned' => array('type' => 'float', 'min' => '0'),
			'double unsigned'           => array('type' => 'float', 'min' => '0'),
			'enum'                      => array('type' => 'string'),
			'fixed'                     => array('type' => 'float', 'exact' => true),
			'fixed unsigned'            => array('type' => 'float', 'exact' => true, 'min' => '0'),
			'float unsigned'            => array('type' => 'float', 'min' => '0'),
			'int unsigned'              => array('type' => 'int', 'min' => '0', 'max' => '4294967295'),
			'integer unsigned'          => array('type' => 'int', 'min' => '0', 'max' => '4294967295'),
			'longblob'                  => array('type' => 'string', 'binary' => true, 'character_maximum_length' => '4294967295'),
			'longtext'                  => array('type' => 'string', 'character_maximum_length' => '4294967295'),
			'mediumblob'                => array('type' => 'string', 'binary' => true, 'character_maximum_length' => '16777215'),
			'mediumint'                 => array('type' => 'int', 'min' => '-8388608', 'max' => '8388607'),
			'mediumint unsigned'        => array('type' => 'int', 'min' => '0', 'max' => '16777215'),
			'mediumtext'                => array('type' => 'string', 'character_maximum_length' => '16777215'),
			'national varchar'          => array('type' => 'string'),
			'numeric unsigned'          => array('type' => 'float', 'exact' => true, 'min' => '0'),
			'nvarchar'                  => array('type' => 'string'),
			'point'                     => array('type' => 'string', 'binary' => true),
			'real unsigned'             => array('type' => 'float', 'min' => '0'),
			'set'                       => array('type' => 'string'),
			'smallint unsigned'         => array('type' => 'int', 'min' => '0', 'max' => '65535'),
			'text'                      => array('type' => 'string', 'character_maximum_length' => '65535'),
			'tinyblob'                  => array('type' => 'string', 'binary' => true, 'character_maximum_length' => '255'),
			'tinyint'                   => array('type' => 'int', 'min' => '-128', 'max' => '127'),
			'tinyint unsigned'          => array('type' => 'int', 'min' => '0', 'max' => '255'),
			'tinytext'                  => array('type' => 'string', 'character_maximum_length' => '255'),
			'varchar'                   => array('type' => 'string', 'exact' => true),
			'year'                      => array('type' => 'string'),
		);

		$type = str_replace(' zerofill', '', $type);

		if (isset($types[$type]))
			return $types[$type];

		return parent::datatype($type);
	}

	public function list_tables($like = null)
	{
		if (is_string($like))
		{
			// Search for table names
			$result = $this->query(\DB::SELECT, 'SHOW TABLES LIKE '.$this->quote($like), false);
		}
		else
		{
			// Find all table names
			$result = $this->query(\DB::SELECT, 'SHOW TABLES', false);
		}

		$tables = array();
		foreach ($result as $row)
		{
			$tables[] = reset($row);
		}

		return $tables;
	}

	public function list_columns($table, $like = null)
	{
		// Quote the table name
		$table = $this->quote_table($table);

		if (is_string($like))
		{
			// Search for column names
			$result = $this->query(\DB::SELECT, 'SHOW FULL COLUMNS FROM '.$table.' LIKE '.$this->quote($like), false);
		}
		else
		{
			// Find all column names
			$result = $this->query(\DB::SELECT, 'SHOW FULL COLUMNS FROM '.$table, false);
		}

		$count = 0;
		$columns = array();
		foreach ($result as $row)
		{
			list($type, $length) = $this->_parse_type($row['Type']);

			$column = $this->datatype($type);

			$column['name']             = $row['Field'];
			$column['default']          = $row['Default'];
			$column['data_type']        = $type;
			$column['null']             = ($row['Null'] == 'YES');
			$column['ordinal_position'] = ++$count;

			switch ($column['type'])
			{
				case 'float':
					if (isset($length))
					{
						list($column['numeric_precision'], $column['numeric_scale']) = explode(',', $length);
					}
				break;
				case 'int':
					if (isset($length))
					{
						// MySQL attribute
						$column['display'] = $length;
					}
				break;
				case 'string':
					switch ($column['data_type'])
					{
						case 'binary':
						case 'varbinary':
							$column['character_maximum_length'] = $length;
						break;

						case 'char':
						case 'varchar':
							$column['character_maximum_length'] = $length;
						case 'text':
						case 'tinytext':
						case 'mediumtext':
						case 'longtext':
							$column['collation_name'] = $row['Collation'];
						break;

						case 'enum':
						case 'set':
							$column['collation_name'] = $row['Collation'];
							$column['options'] = explode('\',\'', substr($length, 1, -1));
						break;
					}
				break;
			}

			// MySQL attributes
			$column['comment']      = $row['Comment'];
			$column['extra']        = $row['Extra'];
			$column['key']          = $row['Key'];
			$column['privileges']   = $row['Privileges'];

			$columns[$row['Field']] = $column;
		}

		return $columns;
	}

	public function escape($value)
	{
		// Make sure the database is connected
		$this->_connection or $this->connect();

		if (($value = $this->_connection->real_escape_string((string) $value)) === false)
		{
			throw new \Database_Exception($this->_connection->error, $this->_connection->errno);
		}

		// SQL standard is to use single-quotes for all values
		return "'$value'";
	}

	public function error_info()
	{
		$errno = $this->_connection->errno;
		return array($errno, empty($errno)? null : $errno, empty($errno) ? null : $this->_connection->error);
	}

	public function in_transaction()
	{
		return $this->_in_transaction;
	}

	public function start_transaction()
	{
		$this->query(0, 'SET AUTOCOMMIT=0', false);
		$this->query(0, 'START TRANSACTION', false);
		$this->_in_transaction = true;
		return true;
	}

	public function commit_transaction()
	{
		$this->query(0, 'COMMIT', false);
		$this->query(0, 'SET AUTOCOMMIT=1', false);
		$this->_in_transaction = false;
		return true;
	}

	public function rollback_transaction()
	{
		$this->query(0, 'ROLLBACK', false);
		$this->query(0, 'SET AUTOCOMMIT=1', false);
		$this->_in_transaction = false;
		return true;
	}

}
