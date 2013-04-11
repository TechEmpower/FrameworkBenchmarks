<?php  if ( ! defined('BASEPATH')) exit('No direct script access allowed');
/**
 * CodeIgniter
 *
 * An open source application development framework for PHP 5.1.6 or newer
 *
 * @package		CodeIgniter
 * @author		ExpressionEngine Dev Team
 * @copyright   Copyright (c) 2008 - 2011, EllisLab, Inc.
 * @license		http://codeigniter.com/user_guide/license.html
 * @link		http://codeigniter.com
 * @since		Version 1.0
 * @filesource
 */

// ------------------------------------------------------------------------

/**
 * oci8 Database Adapter Class
 *
 * Note: _DB is an extender class that the app controller
 * creates dynamically based on whether the active record
 * class is being used or not.
 *
 * @package		CodeIgniter
 * @subpackage  Drivers
 * @category	Database
 * @author		ExpressionEngine Dev Team
 * @link		http://codeigniter.com/user_guide/database/
 */

/**
 * oci8 Database Adapter Class
 *
 * This is a modification of the DB_driver class to
 * permit access to oracle databases
 *
 * @author	  Kelly McArdle
 *
 */

class CI_DB_oci8_driver extends CI_DB {

	var $dbdriver = 'oci8';

	// The character used for excaping
	var $_escape_char = '"';

	// clause and character used for LIKE escape sequences
	var $_like_escape_str = " escape '%s' ";
	var $_like_escape_chr = '!';

	/**
	 * The syntax to count rows is slightly different across different
	 * database engines, so this string appears in each driver and is
	 * used for the count_all() and count_all_results() functions.
	 */
	var $_count_string = "SELECT COUNT(1) AS ";
	var $_random_keyword = ' ASC'; // not currently supported

	// Set "auto commit" by default
	var $_commit = OCI_COMMIT_ON_SUCCESS;

	// need to track statement id and cursor id
	var $stmt_id;
	var $curs_id;

	// if we use a limit, we will add a field that will
	// throw off num_fields later
	var $limit_used;

	/**
	 * Non-persistent database connection
	 *
	 * @access  private called by the base class
	 * @return  resource
	 */
	public function db_connect()
	{
		return @oci_connect($this->username, $this->password, $this->hostname, $this->char_set);
	}

	// --------------------------------------------------------------------

	/**
	 * Persistent database connection
	 *
	 * @access  private called by the base class
	 * @return  resource
	 */
	public function db_pconnect()
	{
		return @oci_pconnect($this->username, $this->password, $this->hostname, $this->char_set);
	}

	// --------------------------------------------------------------------

	/**
	 * Reconnect
	 *
	 * Keep / reestablish the db connection if no queries have been
	 * sent for a length of time exceeding the server's idle timeout
	 *
	 * @access	public
	 * @return	void
	 */
	public function reconnect()
	{
		// not implemented in oracle
		return;
	}

	// --------------------------------------------------------------------

	/**
	 * Select the database
	 *
	 * @access  private called by the base class
	 * @return  resource
	 */
	public function db_select()
	{
		// Not in Oracle - schemas are actually usernames
		return TRUE;
	}

	// --------------------------------------------------------------------

	/**
	 * Set client character set
	 *
	 * @access	public
	 * @param	string
	 * @param	string
	 * @return	resource
	 */
	public function db_set_charset($charset, $collation)
	{
		// @todo - add support if needed
		return TRUE;
	}

	// --------------------------------------------------------------------

	/**
	 * Version number query string
	 *
	 * @access  protected
	 * @return  string
	 */
	protected function _version()
	{
		return oci_server_version($this->conn_id);
	}

	// --------------------------------------------------------------------

	/**
	 * Execute the query
	 *
	 * @access  protected  called by the base class
	 * @param   string  an SQL query
	 * @return  resource
	 */
	protected function _execute($sql)
	{
		// oracle must parse the query before it is run. All of the actions with
		// the query are based on the statement id returned by ociparse
		$this->stmt_id = FALSE;
		$this->_set_stmt_id($sql);
		oci_set_prefetch($this->stmt_id, 1000);
		return @oci_execute($this->stmt_id, $this->_commit);
	}

	/**
	 * Generate a statement ID
	 *
	 * @access  private
	 * @param   string  an SQL query
	 * @return  none
	 */
	private function _set_stmt_id($sql)
	{
		if ( ! is_resource($this->stmt_id))
		{
			$this->stmt_id = oci_parse($this->conn_id, $this->_prep_query($sql));
		}
	}

	// --------------------------------------------------------------------

	/**
	 * Prep the query
	 *
	 * If needed, each database adapter can prep the query string
	 *
	 * @access  private called by execute()
	 * @param   string  an SQL query
	 * @return  string
	 */
	private function _prep_query($sql)
	{
		return $sql;
	}

	// --------------------------------------------------------------------

	/**
	 * getCursor.  Returns a cursor from the datbase
	 *
	 * @access  public
	 * @return  cursor id
	 */
	public function get_cursor()
	{
		$this->curs_id = oci_new_cursor($this->conn_id);
		return $this->curs_id;
	}

	// --------------------------------------------------------------------

	/**
	 * Stored Procedure.  Executes a stored procedure
	 *
	 * @access  public
	 * @param   package	 package stored procedure is in
	 * @param   procedure   stored procedure to execute
	 * @param   params	  array of parameters
	 * @return  array
	 *
	 * params array keys
	 *
	 * KEY	  OPTIONAL	NOTES
	 * name		no		the name of the parameter should be in :<param_name> format
	 * value	no		the value of the parameter.  If this is an OUT or IN OUT parameter,
	 *					this should be a reference to a variable
	 * type		yes		the type of the parameter
	 * length	yes		the max size of the parameter
	 */
	public function stored_procedure($package, $procedure, $params)
	{
		if ($package == '' OR $procedure == '' OR ! is_array($params))
		{
			if ($this->db_debug)
			{
				log_message('error', 'Invalid query: '.$package.'.'.$procedure);
				return $this->display_error('db_invalid_query');
			}
			return FALSE;
		}

		// build the query string
		$sql = "begin $package.$procedure(";

		$have_cursor = FALSE;
		foreach ($params as $param)
		{
			$sql .= $param['name'] . ",";

			if (array_key_exists('type', $param) && ($param['type'] === OCI_B_CURSOR))
			{
				$have_cursor = TRUE;
			}
		}
		$sql = trim($sql, ",") . "); end;";

		$this->stmt_id = FALSE;
		$this->_set_stmt_id($sql);
		$this->_bind_params($params);
		$this->query($sql, FALSE, $have_cursor);
	}

	// --------------------------------------------------------------------

	/**
	 * Bind parameters
	 *
	 * @access  private
	 * @return  none
	 */
	private function _bind_params($params)
	{
		if ( ! is_array($params) OR ! is_resource($this->stmt_id))
		{
			return;
		}

		foreach ($params as $param)
		{
			foreach (array('name', 'value', 'type', 'length') as $val)
			{
				if ( ! isset($param[$val]))
				{
					$param[$val] = '';
				}
			}

			oci_bind_by_name($this->stmt_id, $param['name'], $param['value'], $param['length'], $param['type']);
		}
	}

	// --------------------------------------------------------------------

	/**
	 * Begin Transaction
	 *
	 * @access	public
	 * @return	bool
	 */
	public function trans_begin($test_mode = FALSE)
	{
		if ( ! $this->trans_enabled)
		{
			return TRUE;
		}

		// When transactions are nested we only begin/commit/rollback the outermost ones
		if ($this->_trans_depth > 0)
		{
			return TRUE;
		}

		// Reset the transaction failure flag.
		// If the $test_mode flag is set to TRUE transactions will be rolled back
		// even if the queries produce a successful result.
		$this->_trans_failure = ($test_mode === TRUE) ? TRUE : FALSE;

		$this->_commit = OCI_DEFAULT;
		return TRUE;
	}

	// --------------------------------------------------------------------

	/**
	 * Commit Transaction
	 *
	 * @access	public
	 * @return	bool
	 */
	public function trans_commit()
	{
		if ( ! $this->trans_enabled)
		{
			return TRUE;
		}

		// When transactions are nested we only begin/commit/rollback the outermost ones
		if ($this->_trans_depth > 0)
		{
			return TRUE;
		}

		$ret = oci_commit($this->conn_id);
		$this->_commit = OCI_COMMIT_ON_SUCCESS;
		return $ret;
	}

	// --------------------------------------------------------------------

	/**
	 * Rollback Transaction
	 *
	 * @access	public
	 * @return	bool
	 */
	public function trans_rollback()
	{
		if ( ! $this->trans_enabled)
		{
			return TRUE;
		}

		// When transactions are nested we only begin/commit/rollback the outermost ones
		if ($this->_trans_depth > 0)
		{
			return TRUE;
		}

		$ret = oci_rollback($this->conn_id);
		$this->_commit = OCI_COMMIT_ON_SUCCESS;
		return $ret;
	}

	// --------------------------------------------------------------------

	/**
	 * Escape String
	 *
	 * @access  public
	 * @param   string
	 * @param	bool	whether or not the string will be used in a LIKE condition
	 * @return  string
	 */
	public function escape_str($str, $like = FALSE)
	{
		if (is_array($str))
		{
			foreach ($str as $key => $val)
			{
				$str[$key] = $this->escape_str($val, $like);
			}

			return $str;
		}

		$str = remove_invisible_characters($str);

		// escape LIKE condition wildcards
		if ($like === TRUE)
		{
			$str = str_replace(	array('%', '_', $this->_like_escape_chr),
								array($this->_like_escape_chr.'%', $this->_like_escape_chr.'_', $this->_like_escape_chr.$this->_like_escape_chr),
								$str);
		}

		return $str;
	}

	// --------------------------------------------------------------------

	/**
	 * Affected Rows
	 *
	 * @access  public
	 * @return  integer
	 */
	public function affected_rows()
	{
		return @oci_num_rows($this->stmt_id);
	}

	// --------------------------------------------------------------------

	/**
	 * Insert ID
	 *
	 * @access  public
	 * @return  integer
	 */
	public function insert_id()
	{
		// not supported in oracle
		return $this->display_error('db_unsupported_function');
	}

	// --------------------------------------------------------------------

	/**
	 * "Count All" query
	 *
	 * Generates a platform-specific query string that counts all records in
	 * the specified database
	 *
	 * @access  public
	 * @param   string
	 * @return  string
	 */
	public function count_all($table = '')
	{
		if ($table == '')
		{
			return 0;
		}

		$query = $this->query($this->_count_string . $this->_protect_identifiers('numrows') . " FROM " . $this->_protect_identifiers($table, TRUE, NULL, FALSE));

		if ($query == FALSE)
		{
			return 0;
		}

		$row = $query->row();
		$this->_reset_select();
		return (int) $row->numrows;
	}

	// --------------------------------------------------------------------

	/**
	 * Show table query
	 *
	 * Generates a platform-specific query string so that the table names can be fetched
	 *
	 * @access	protected
	 * @param	boolean
	 * @return	string
	 */
	protected function _list_tables($prefix_limit = FALSE)
	{
		$sql = "SELECT TABLE_NAME FROM ALL_TABLES";

		if ($prefix_limit !== FALSE AND $this->dbprefix != '')
		{
			$sql .= " WHERE TABLE_NAME LIKE '".$this->escape_like_str($this->dbprefix)."%' ".sprintf($this->_like_escape_str, $this->_like_escape_chr);
		}

		return $sql;
	}

	// --------------------------------------------------------------------

	/**
	 * Show column query
	 *
	 * Generates a platform-specific query string so that the column names can be fetched
	 *
	 * @access  protected
	 * @param   string  the table name
	 * @return  string
	 */
	protected function _list_columns($table = '')
	{
		return "SELECT COLUMN_NAME FROM all_tab_columns WHERE table_name = '$table'";
	}

	// --------------------------------------------------------------------

	/**
	 * Field data query
	 *
	 * Generates a platform-specific query so that the column data can be retrieved
	 *
	 * @access  public
	 * @param   string  the table name
	 * @return  object
	 */
	protected function _field_data($table)
	{
		return "SELECT * FROM ".$table." where rownum = 1";
	}

	// --------------------------------------------------------------------

	/**
	 * The error message string
	 *
	 * @access  protected
	 * @return  string
	 */
	protected function _error_message()
	{
		// If the error was during connection, no conn_id should be passed
		$error = is_resource($this->conn_id) ? oci_error($this->conn_id) : oci_error();
		return $error['message'];
	}

	// --------------------------------------------------------------------

	/**
	 * The error message number
	 *
	 * @access  protected
	 * @return  integer
	 */
	protected function _error_number()
	{
		// Same as _error_message()
		$error = is_resource($this->conn_id) ? oci_error($this->conn_id) : oci_error();
		return $error['code'];
	}

	// --------------------------------------------------------------------

	/**
	 * Escape the SQL Identifiers
	 *
	 * This function escapes column and table names
	 *
	 * @access	protected
	 * @param	string
	 * @return	string
	 */
	protected function _escape_identifiers($item)
	{
		if ($this->_escape_char == '')
		{
			return $item;
		}

		foreach ($this->_reserved_identifiers as $id)
		{
			if (strpos($item, '.'.$id) !== FALSE)
			{
				$str = $this->_escape_char. str_replace('.', $this->_escape_char.'.', $item);

				// remove duplicates if the user already included the escape
				return preg_replace('/['.$this->_escape_char.']+/', $this->_escape_char, $str);
			}
		}

		if (strpos($item, '.') !== FALSE)
		{
			$str = $this->_escape_char.str_replace('.', $this->_escape_char.'.'.$this->_escape_char, $item).$this->_escape_char;
		}
		else
		{
			$str = $this->_escape_char.$item.$this->_escape_char;
		}

		// remove duplicates if the user already included the escape
		return preg_replace('/['.$this->_escape_char.']+/', $this->_escape_char, $str);
	}

	// --------------------------------------------------------------------

	/**
	 * From Tables
	 *
	 * This function implicitly groups FROM tables so there is no confusion
	 * about operator precedence in harmony with SQL standards
	 *
	 * @access	protected
	 * @param	type
	 * @return	type
	 */
	protected function _from_tables($tables)
	{
		if ( ! is_array($tables))
		{
			$tables = array($tables);
		}

		return implode(', ', $tables);
	}

	// --------------------------------------------------------------------

	/**
	 * Insert statement
	 *
	 * Generates a platform-specific insert string from the supplied data
	 *
	 * @access  public
	 * @param   string  the table name
	 * @param   array   the insert keys
	 * @param   array   the insert values
	 * @return  string
	 */
	protected function _insert($table, $keys, $values)
	{
		return "INSERT INTO ".$table." (".implode(', ', $keys).") VALUES (".implode(', ', $values).")";
	}

	// --------------------------------------------------------------------

	/**
	 * Insert_batch statement
	 *
	 * Generates a platform-specific insert string from the supplied data
	 *
	 * @access      protected
	 * @param       string  the table name
	 * @param       array   the insert keys
	 * @param       array   the insert values
	 * @return      string
	 */
	protected function _insert_batch($table, $keys, $values)
	{
		$keys = implode(', ', $keys);
		$sql = "INSERT ALL\n";

		for ($i = 0, $c = count($values); $i < $c; $i++)
		{
			$sql .= '	INTO ' . $table . ' (' . $keys . ') VALUES ' . $values[$i] . "\n";
		}

		$sql .= 'SELECT * FROM dual';

		return $sql;
	}

	// --------------------------------------------------------------------

	/**
	 * Update statement
	 *
	 * Generates a platform-specific update string from the supplied data
	 *
	 * @access	protected
	 * @param	string	the table name
	 * @param	array	the update data
	 * @param	array	the where clause
	 * @param	array	the orderby clause
	 * @param	array	the limit clause
	 * @return	string
	 */
	protected function _update($table, $values, $where, $orderby = array(), $limit = FALSE)
	{
		foreach ($values as $key => $val)
		{
			$valstr[] = $key." = ".$val;
		}

		$limit = ( ! $limit) ? '' : ' LIMIT '.$limit;

		$orderby = (count($orderby) >= 1)?' ORDER BY '.implode(", ", $orderby):'';

		$sql = "UPDATE ".$table." SET ".implode(', ', $valstr);

		$sql .= ($where != '' AND count($where) >=1) ? " WHERE ".implode(" ", $where) : '';

		$sql .= $orderby.$limit;

		return $sql;
	}

	// --------------------------------------------------------------------

	/**
	 * Truncate statement
	 *
	 * Generates a platform-specific truncate string from the supplied data
	 * If the database does not support the truncate() command
	 * This function maps to "DELETE FROM table"
	 *
	 * @access	protected
	 * @param	string	the table name
	 * @return	string
	 */
	protected function _truncate($table)
	{
		return "TRUNCATE TABLE ".$table;
	}

	// --------------------------------------------------------------------

	/**
	 * Delete statement
	 *
	 * Generates a platform-specific delete string from the supplied data
	 *
	 * @access	protected
	 * @param	string	the table name
	 * @param	array	the where clause
	 * @param	string	the limit clause
	 * @return	string
	 */
	protected function _delete($table, $where = array(), $like = array(), $limit = FALSE)
	{
		$conditions = '';

		if (count($where) > 0 OR count($like) > 0)
		{
			$conditions = "\nWHERE ";
			$conditions .= implode("\n", $this->ar_where);

			if (count($where) > 0 && count($like) > 0)
			{
				$conditions .= " AND ";
			}
			$conditions .= implode("\n", $like);
		}

		$limit = ( ! $limit) ? '' : ' LIMIT '.$limit;

		return "DELETE FROM ".$table.$conditions.$limit;
	}

	// --------------------------------------------------------------------

	/**
	 * Limit string
	 *
	 * Generates a platform-specific LIMIT clause
	 *
	 * @access  protected
	 * @param   string  the sql query string
	 * @param   integer the number of rows to limit the query to
	 * @param   integer the offset value
	 * @return  string
	 */
	protected function _limit($sql, $limit, $offset)
	{
		$limit = $offset + $limit;
		$newsql = "SELECT * FROM (select inner_query.*, rownum rnum FROM ($sql) inner_query WHERE rownum < $limit)";

		if ($offset != 0)
		{
			$newsql .= " WHERE rnum >= $offset";
		}

		// remember that we used limits
		$this->limit_used = TRUE;

		return $newsql;
	}

	// --------------------------------------------------------------------

	/**
	 * Close DB Connection
	 *
	 * @access  protected
	 * @param   resource
	 * @return  void
	 */
	protected function _close($conn_id)
	{
		@oci_close($conn_id);
	}


}



/* End of file oci8_driver.php */
/* Location: ./system/database/drivers/oci8/oci8_driver.php */
