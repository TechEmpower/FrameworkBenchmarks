<?php

namespace PHPixie\DB\Mysql;

/**
 * Mysqli Database Implementation
 * @package Database
 */
class Connection extends \PHPixie\DB\Connection
{
	
	/**
	 * Mysqli database connection object
	 * @var mysqli
	 * @link http://php.net/manual/en/class.mysqli.php
	 */
	public $conn;

	/**
	 * Type of the database, mysql.
	 * @var string
	 */
	public $db_type = 'mysql';

	/**
	 * Initializes database connection
	 *
	 * @param string $config Name of the connection to initialize
	 * @return void
	 */
	public function __construct($pixie, $config)
	{
		parent::__construct($pixie, $config);
		
		$this->conn = mysqli_connect(
			$pixie->config->get("db.{$config}.host", 'localhost'),
			$pixie->config->get("db.{$config}.user", ''),
			$pixie->config->get("db.{$config}.password", ''),
			$pixie->config->get("db.{$config}.db")
		);
		$this->conn->set_charset("utf8");
	}

	/**
	 * Gets column names for the specified table
	 *
	 * @param string $table Name of the table to get columns from
	 * @return array Array of column names
	 * @throw \Exception if table doesn't exist
	 */
	public function list_columns($table)
	{
		$columns = array();
		$table_desc = $this->execute("DESCRIBE `$table`");
		if (!$table_desc->valid())
		{
			throw new \Exception("Table '{$table}' doesn't exist");
		}
		foreach ($table_desc as $column)
		{
			$columns[] = $column->Field;
		}

		return $columns;
	}

	/**
	 * Builds a new Query implementation
	 *
	 * @param string $type Query type. Available types: select,update,insert,delete,count
	 * @return Query_Mysql_Driver  Returns a Mysqli implementation of a Query.
	 * @see Query_Database
	 */
	public function query($type)
	{
		return $this->pixie->db->query_driver('Mysql', $this, $type);
	}

	/**
	 * Gets the id of the last inserted row.
	 *
	 * @return mixed Row id
	 */
	public function insert_id()
	{
		return $this->conn->insert_id;
	}

	/**
	 * Executes a prepared statement query
	 *
	 * @param string   $query  A prepared statement query
	 * @param array     $params Parameters for the query
	 * @return Result_Mysql_Driver    Mysqli implementation of a database result
	 * @throws \Exception If the query resulted in an error
	 * @see Database_Result
	 */
	public function execute($query, $params = array())
	{
		$cursor = $this->conn->prepare($query);
		if (!$cursor)
			throw new \Exception("Database error: {$this->conn->error} \n in query:\n{$query}");
		$types = '';
		$bind = array();
		$refs = array();
		if (!empty($params))
		{
			foreach ($params as $key => $param)
			{
				$refs[$key] = is_array($param) ? $param[0] : $param;
				$bind[] = &$refs[$key];
				$types .= is_array($param) ? $param[1] : 's';
			}
			array_unshift($bind, $types);

			call_user_func_array(array($cursor, 'bind_param'), $bind);
		}
		$cursor->execute();
		$res = $cursor->get_result();
		return $this->pixie->db->result_driver('Mysql', $res);
	}

}
