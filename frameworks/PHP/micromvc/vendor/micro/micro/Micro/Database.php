<?php
/**
 * Database
 *
 * Provides a database wrapper around the PDO service to help reduce the effort
 * to interact with a data source.
 *
 * @package		MicroMVC
 * @author		David Pennington
 * @copyright	(c) 2011 MicroMVC Framework
 * @license		http://micromvc.com/license
 ********************************** 80 Columns *********************************
 */
namespace Micro;

class Database
{

	public $pdo = NULL;

	public $type = NULL;

	public $i = '"';

	public $statements = array();

	protected $config = array();

	public static $queries = array();

	public static $last_query = NULL;

	/**
	 * Set the database type and save the config for later.
	 *
	 * @param array $config
	 */
	public function __construct(array $config)
	{
		// Auto-detect database type from DNS
		$this->type = current(explode(':', $config['dns'], 2));

		// Save config for connection
		$this->config = $config;

		// MySQL uses a non-standard column identifier
		if($this->type == 'mysql') $this->i = '`';
	}


	/**
	 * Database lazy-loading to setup connection only when finally needed
	 */
	public function connect()
	{
		extract($this->config);

		// Clear config for security reasons
		$this->config = NULL;

		// Connect to PDO
		$this->pdo = new \PDO($dns, $username, $password, $params);

		// PDO should throw exceptions
		$this->pdo->setAttribute(\PDO::ATTR_ERRMODE, \PDO::ERRMODE_EXCEPTION);
	}


	/**
	 * Quotes a string for use in a query
	 *
	 * @param mixed $value to quote
	 * @return string
	 */
	public function quote($value)
	{
		if( ! $this->pdo) $this->connect();
		return $this->pdo->quote($value);
	}


	/**
	 * Run a SQL query and return a single column (i.e. COUNT(*) queries)
	 *
	 * @param string $sql query to run
	 * @param array $params the prepared query params
	 * @param int $column the optional column to return
	 * @return mixed
	 */
	public function column($sql, array $params = NULL, $column = 0)
	{
		// If the query succeeds, fetch the column
		return ($statement = $this->query($sql, $params)) ? $statement->fetchColumn($column) : NULL;
	}


	/**
	 * Run a SQL query and return a single row object
	 *
	 * @param string $sql query to run
	 * @param array $params the prepared query params
	 * @param string $object the optional name of the class for this row
	 * @return array
	 */
	public function row($sql, array $params = NULL, $object = NULL)
	{
		if( ! $statement = $this->query($sql, $params)) return;

		$row = $statement->fetch(\PDO::FETCH_OBJ);

		// If they want the row returned as a custom object
		if($object) $row = new $object($row);

		return $row;
	}


	/**
	 * Run a SQL query and return an array of row objects or an array
	 * consisting of all values of a single column.
	 *
	 * @param string $sql query to run
	 * @param array $params the optional prepared query params
	 * @param int $column the optional column to return
	 * @return array
	 */
	public function fetch($sql, array $params = NULL, $column = NULL)
	{
		if( ! $statement = $this->query($sql, $params)) return;

		// Return an array of records
		if($column === NULL) return $statement->fetchAll(\PDO::FETCH_OBJ);

		// Fetch a certain column from all rows
		return $statement->fetchAll(\PDO::FETCH_COLUMN , $column);
	}


	/**
	 * Run a SQL query and return the statement object
	 *
	 * @param string $sql query to run
	 * @param array $params the prepared query params
	 * @return PDOStatement
	 */
	public function query($sql, array $params = NULL, $cache_statement = FALSE)
	{
		$time = microtime(TRUE);

		self::$last_query = $sql;

		// Connect if needed
		if( ! $this->pdo) $this->connect();

		// Should we cached PDOStatements? (Best for batch inserts/updates)
		if($cache_statement)
		{
			$hash = md5($sql);

			if(isset($this->statements[$hash]))
			{
				$statement = $this->statements[$hash];
			}
			else
			{
				$statement = $this->statements[$hash] = $this->pdo->prepare($sql);
			}
		}
		else
		{
			$statement = $this->pdo->prepare($sql);
		}

		$statement->execute($params);
		//$statement = $this->pdo->query($sql);

		// Save query results by database type
		self::$queries[$this->type][] = array(microtime(TRUE) - $time, $sql);

		return $statement;
	}


	/**
	 * Run a DELETE SQL query and return the number of rows deleted
	 *
	 * @param string $sql query to run
	 * @param array $params the prepared query params
	 * @return int
	 */
	public function delete($sql, array $params = NULL)
	{
		if($statement = $this->query($sql, $params))
		{
			return $statement->rowCount();
		}
	}


	/**
	 * Creates and runs an INSERT statement using the values provided
	 *
	 * @param string $table the table name
	 * @param array $data the column => value pairs
	 * @return int
	 */
	public function insert($table, array $data, $cache_statement = TRUE)
	{
		$sql = $this->insert_sql($table, $data);

		// PostgreSQL does not return the ID by default
		if($this->type == 'pgsql')
		{
			// Insert record and return the whole row (the "id" field may not exist)
			if($statement = $this->query($sql.' RETURNING "id"', array_values($data)))
			{
				// The first column *should* be the ID
				return $statement->fetchColumn(0);
			}

			return;
		}

		// Insert data and return the new row's ID
		return $this->query($sql, array_values($data), $cache_statement) ? $this->pdo->lastInsertId() : NULL;
	}


	/**
	 * Create insert SQL
	 *
	 * @param array $data row data
	 * @return string
	 */
	public function insert_sql($table, $data)
	{
		$i = $this->i;

		// Column names come from the array keys
		$columns = implode("$i, $i", array_keys($data));

		// Build prepared statement SQL
		return "INSERT INTO $i$table$i ($i".$columns."$i) VALUES (" . rtrim(str_repeat('?, ', count($data)), ', ') . ')';
	}


	/**
	 * Builds an UPDATE statement using the values provided.
	 * Create a basic WHERE section of a query using the format:
	 * array('column' => $value) or array("column = $value")
	 *
	 * @param string $table the table name
	 * @param array $data the column => value pairs
	 * @return int
	 */
	public function update($table, $data, array $where = NULL, $cache_statement = TRUE)
	{
		$i = $this->i;

		// Column names come from the array keys
		$columns = implode("$i = ?, $i", array_keys($data));

		// Build prepared statement SQL
		$sql = "UPDATE $i$table$i SET $i" . $columns . "$i = ? WHERE ";

		// Process WHERE conditions
		list($where, $params) = $this->where($where);

		// Append WHERE conditions to query and statement params
		if($statement = $this->query($sql . $where, array_merge(array_values($data), $params), $cache_statement))
		{
			return $statement->rowCount();
		}
	}


	/**
	 * Create a basic,  single-table SQL query
	 *
	 * @param string $columns
	 * @param string $table
	 * @param array $where array of conditions
	 * @param int $limit
	 * @param int $offset
	 * @param array $order array of order by conditions
	 * @return array
	 */
	public function select($column, $table, $where = NULL, $limit = NULL, $offset = 0, $order = NULL)
	{
		$i = $this->i;

		$sql = "SELECT $column FROM $i$table$i";

		// Process WHERE conditions
		list($where, $params) = $this->where($where);

		// If there are any conditions, append them
		if($where) $sql .= " WHERE $where";

		// Append optional ORDER BY sorting
		$sql .= self::order_by($order);

		if($limit)
		{
			// MySQL/SQLite use a different LIMIT syntax
			$sql .= $this->type == 'pgsql' ? " LIMIT $limit OFFSET $offset" : " LIMIT $offset, $limit";
		}

		return array($sql, $params);
	}


	/**
	 * Generate the SQL WHERE clause options from an array
	 *
	 * @param array $where array of column => $value indexes
	 * @return array
	 */
	public function where($where = NULL)
	{
		$a = $s = array();

		if($where)
		{
			$i = $this->i;

			foreach($where as $c => $v)
			{
				// Raw WHERE conditions are allowed array(0 => '"a" = NOW()')
				if(is_int($c))
				{
					$s[] = $v;
				}
				else
				{
					// Column => Value
					$s[] = "$i$c$i = ?";
					$a[] = $v;
				}
			}
		}

		// Return an array with the SQL string + params
		return array(implode(' AND ', $s), $a);
	}


	/**
	 * Create the ORDER BY clause for MySQL and SQLite (still working on PostgreSQL)
	 *
	 * @param array $fields to order by
	 */
	public function order_by($fields = NULL)
	{
		if( ! $fields) return;

		$i = $this->i;

		$sql = ' ORDER BY ';

		// Add each order clause
		foreach($fields as $k => $v) $sql .= "$i$k$i $v, ";

		// Remove ending ", "
		return substr($sql, 0, -2);
	}

}

// END
