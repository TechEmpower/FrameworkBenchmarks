<?php

namespace PHPixie\DB\PDO;

/**
 * PDO Database implementation.
 * @package Database
 */
class Connection extends \PHPixie\DB\Connection
{

	public $pixie;
	
	/**
	 * Connection object
	 * @var PDO
	 * @link http://php.net/manual/en/class.pdo.php
	 */
	public $conn;

	/**
	 * Type of the database, e.g. mysql, pgsql etc.
	 * @var string
	 */
	public $db_type;

	/**
	 * Initializes database connection
	 *
	 * @param string $config Name of the connection to initialize
	 * @return void
	 */
	public function __construct($pixie, $config)
	{
		parent::__construct($pixie, $config);
		
		$this->conn = new \PDO(
			$pixie->config->get("db.{$config}.connection"),
			$pixie->config->get("db.{$config}.user", ''),
			$pixie->config->get("db.{$config}.password", '')
		);
		$this->conn->setAttribute(\PDO::ATTR_ERRMODE, \PDO::ERRMODE_EXCEPTION);
		$this->db_type = strtolower(str_replace('PDO_', '', $this->conn->getAttribute(\PDO::ATTR_DRIVER_NAME)));
		if ($this->db_type != 'sqlite')
		{
			$this->conn->exec("SET NAMES utf8");
		}
	}

	/**
	 * Builds a new Query implementation
	 *
	 * @param string $type Query type. Available types: select,update,insert,delete,count
	 * @return Query_PDO_Driver  Returns a PDO implementation of a Query.
	 * @see Query_Database
	 */
	public function query($type)
	{
		return $this->pixie->db->query_driver('PDO', $this, $type);
	}

	/**
	 * Gets the id of the last inserted row.
	 *
	 * @return mixed Row id
	 */
	public function insert_id()
	{
		if ($this->db_type == 'pgsql')
		{
			return $this->execute('SELECT lastval() as id')->current()->id;
		}
		return $this->conn->lastInsertId();
	}

	/**
	 * Gets column names for the specified table
	 *
	 * @param string $table Name of the table to get columns from
	 * @return array Array of column names
	 */
	public function list_columns($table)
	{
		$columns = array();
		if ($this->db_type == 'mysql')
		{
			$table_desc = $this->execute("DESCRIBE `$table`");
			foreach ($table_desc as $column)
			{
				$columns[] = $column->Field;
			}
		}
		if ($this->db_type == 'pgsql')
		{
			$table_desc = $this->execute("select column_name from information_schema.columns where table_name = '{$table}' and table_catalog=current_database();");
			foreach ($table_desc as $column)
			{
				$columns[] = $column->column_name;
			}
		}
		if ($this->db_type == 'sqlite')
		{
			$table_desc = $this->execute("PRAGMA table_info('$table')");
			foreach ($table_desc as $column)
			{
				$columns[] = $column->name;
			}
		}
		return $columns;
	}

	/**
	 * Executes a prepared statement query
	 *
	 * @param string   $query  A prepared statement query
	 * @param array     $params Parameters for the query
	 * @return Result_PDO_Driver    PDO implementation of a database result
	 * @throws \Exception If the query resulted in an error
	 * @see Database_Result
	 */
	public function execute($query, $params = array())
	{
		$cursor = $this->conn->prepare($query);
		if (!$cursor->execute($params))
		{
			$error = $cursor->errorInfo();
			throw new Exception("Database error:\n".$error[2]." \n in query:\n{$query}");
		}
		return $this->pixie->db->result_driver('PDO', $cursor);
	}

}
