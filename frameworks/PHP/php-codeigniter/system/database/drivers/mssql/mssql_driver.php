<?php  if ( ! defined('BASEPATH')) exit('No direct script access allowed');
/**
 * CodeIgniter
 *
 * An open source application development framework for PHP 5.1.6 or newer
 *
 * @package		CodeIgniter
 * @author		ExpressionEngine Dev Team
 * @copyright	Copyright (c) 2008 - 2011, EllisLab, Inc.
 * @license		http://codeigniter.com/user_guide/license.html
 * @link		http://codeigniter.com
 * @since		Version 1.0
 * @filesource
 */

// ------------------------------------------------------------------------

/**
 * MS SQL Database Adapter Class
 *
 * Note: _DB is an extender class that the app controller
 * creates dynamically based on whether the active record
 * class is being used or not.
 *
 * @package		CodeIgniter
 * @subpackage	Drivers
 * @category	Database
 * @author		ExpressionEngine Dev Team
 * @link		http://codeigniter.com/user_guide/database/
 */
class CI_DB_mssql_driver extends CI_DB {

	var $dbdriver = 'mssql';

	// The character used for escaping
	var $_escape_char = '';

	// clause and character used for LIKE escape sequences
	var $_like_escape_str = " ESCAPE '%s' ";
	var $_like_escape_chr = '!';

	/**
	 * The syntax to count rows is slightly different across different
	 * database engines, so this string appears in each driver and is
	 * used for the count_all() and count_all_results() functions.
	 */
	var $_count_string = "SELECT COUNT(*) AS ";
	var $_random_keyword = ' ASC'; // not currently supported

	/**
	 * Non-persistent database connection
	 *
	 * @access	private called by the base class
	 * @return	resource
	 */
	function db_connect()
	{
		if ($this->port != '')
		{
			$this->hostname .= ','.$this->port;
		}

		return @mssql_connect($this->hostname, $this->username, $this->password);
	}

	// --------------------------------------------------------------------

	/**
	 * Persistent database connection
	 *
	 * @access	private called by the base class
	 * @return	resource
	 */
	function db_pconnect()
	{
		if ($this->port != '')
		{
			$this->hostname .= ','.$this->port;
		}

		return @mssql_pconnect($this->hostname, $this->username, $this->password);
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
	function reconnect()
	{
		// not implemented in MSSQL
	}

	// --------------------------------------------------------------------

	/**
	 * Select the database
	 *
	 * @access	private called by the base class
	 * @return	resource
	 */
	function db_select()
	{
		// Note: The brackets are required in the event that the DB name
		// contains reserved characters
		return @mssql_select_db('['.$this->database.']', $this->conn_id);
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
	function db_set_charset($charset, $collation)
	{
		// @todo - add support if needed
		return TRUE;
	}

	// --------------------------------------------------------------------

	/**
	 * Execute the query
	 *
	 * @access	private called by the base class
	 * @param	string	an SQL query
	 * @return	resource
	 */
	function _execute($sql)
	{
		$sql = $this->_prep_query($sql);
		return @mssql_query($sql, $this->conn_id);
	}

	// --------------------------------------------------------------------

	/**
	 * Prep the query
	 *
	 * If needed, each database adapter can prep the query string
	 *
	 * @access	private called by execute()
	 * @param	string	an SQL query
	 * @return	string
	 */
	function _prep_query($sql)
	{
		return $sql;
	}

	// --------------------------------------------------------------------

	/**
	 * Begin Transaction
	 *
	 * @access	public
	 * @return	bool
	 */
	function trans_begin($test_mode = FALSE)
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

		$this->simple_query('BEGIN TRAN');
		return TRUE;
	}

	// --------------------------------------------------------------------

	/**
	 * Commit Transaction
	 *
	 * @access	public
	 * @return	bool
	 */
	function trans_commit()
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

		$this->simple_query('COMMIT TRAN');
		return TRUE;
	}

	// --------------------------------------------------------------------

	/**
	 * Rollback Transaction
	 *
	 * @access	public
	 * @return	bool
	 */
	function trans_rollback()
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

		$this->simple_query('ROLLBACK TRAN');
		return TRUE;
	}

	// --------------------------------------------------------------------

	/**
	 * Escape String
	 *
	 * @access	public
	 * @param	string
	 * @param	bool	whether or not the string will be used in a LIKE condition
	 * @return	string
	 */
	function escape_str($str, $like = FALSE)
	{
		if (is_array($str))
		{
			foreach ($str as $key => $val)
			{
				$str[$key] = $this->escape_str($val, $like);
			}

			return $str;
		}

		// Escape single quotes
		$str = str_replace("'", "''", remove_invisible_characters($str));

		// escape LIKE condition wildcards
		if ($like === TRUE)
		{
			$str = str_replace(
				array($this->_like_escape_chr, '%', '_'),
				array($this->_like_escape_chr.$this->_like_escape_chr, $this->_like_escape_chr.'%', $this->_like_escape_chr.'_'),
				$str
			);
		}

		return $str;
	}

	// --------------------------------------------------------------------

	/**
	 * Affected Rows
	 *
	 * @access	public
	 * @return	integer
	 */
	function affected_rows()
	{
		return @mssql_rows_affected($this->conn_id);
	}

	// --------------------------------------------------------------------

	/**
	* Insert ID
	*
	* Returns the last id created in the Identity column.
	*
	* @access public
	* @return integer
	*/
	function insert_id()
	{
		$ver = self::_parse_major_version($this->version());
		$sql = ($ver >= 8 ? "SELECT SCOPE_IDENTITY() AS last_id" : "SELECT @@IDENTITY AS last_id");
		$query = $this->query($sql);
		$row = $query->row();
		return $row->last_id;
	}

	// --------------------------------------------------------------------

	/**
	* Parse major version
	*
	* Grabs the major version number from the
	* database server version string passed in.
	*
	* @access private
	* @param string $version
	* @return int16 major version number
	*/
	function _parse_major_version($version)
	{
		preg_match('/([0-9]+)\.([0-9]+)\.([0-9]+)/', $version, $ver_info);
		return $ver_info[1]; // return the major version b/c that's all we're interested in.
	}

	// --------------------------------------------------------------------

	/**
	* Version number query string
	*
	* @access public
	* @return string
	*/
	function _version()
	{
		return "SELECT @@VERSION AS ver";
	}

	// --------------------------------------------------------------------

	/**
	 * "Count All" query
	 *
	 * Generates a platform-specific query string that counts all records in
	 * the specified database
	 *
	 * @access	public
	 * @param	string
	 * @return	string
	 */
	function count_all($table = '')
	{
		if ($table == '')
		{
			return 0;
		}

		$query = $this->query($this->_count_string . $this->_protect_identifiers('numrows') . " FROM " . $this->_protect_identifiers($table, TRUE, NULL, FALSE));

		if ($query->num_rows() == 0)
		{
			return 0;
		}

		$row = $query->row();
		$this->_reset_select();
		return (int) $row->numrows;
	}

	// --------------------------------------------------------------------

	/**
	 * List table query
	 *
	 * Generates a platform-specific query string so that the table names can be fetched
	 *
	 * @access	private
	 * @param	boolean
	 * @return	string
	 */
	function _list_tables($prefix_limit = FALSE)
	{
		$sql = "SELECT name FROM sysobjects WHERE type = 'U' ORDER BY name";

		// for future compatibility
		if ($prefix_limit !== FALSE AND $this->dbprefix != '')
		{
			//$sql .= " LIKE '".$this->escape_like_str($this->dbprefix)."%' ".sprintf($this->_like_escape_str, $this->_like_escape_chr);
			return FALSE; // not currently supported
		}

		return $sql;
	}

	// --------------------------------------------------------------------

	/**
	 * List column query
	 *
	 * Generates a platform-specific query string so that the column names can be fetched
	 *
	 * @access	private
	 * @param	string	the table name
	 * @return	string
	 */
	function _list_columns($table = '')
	{
		return "SELECT * FROM INFORMATION_SCHEMA.Columns WHERE TABLE_NAME = '".$table."'";
	}

	// --------------------------------------------------------------------

	/**
	 * Field data query
	 *
	 * Generates a platform-specific query so that the column data can be retrieved
	 *
	 * @access	public
	 * @param	string	the table name
	 * @return	object
	 */
	function _field_data($table)
	{
		return "SELECT TOP 1 * FROM ".$table;
	}

	// --------------------------------------------------------------------

	/**
	 * The error message string
	 *
	 * @access	private
	 * @return	string
	 */
	function _error_message()
	{
		return mssql_get_last_message();
	}

	// --------------------------------------------------------------------

	/**
	 * The error message number
	 *
	 * @access	private
	 * @return	integer
	 */
	function _error_number()
	{
		// Are error numbers supported?
		return '';
	}

	// --------------------------------------------------------------------

	/**
	 * Escape the SQL Identifiers
	 *
	 * This function escapes column and table names
	 *
	 * @access	private
	 * @param	string
	 * @return	string
	 */
	function _escape_identifiers($item)
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
	 * @access	public
	 * @param	type
	 * @return	type
	 */
	function _from_tables($tables)
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
	 * @access	public
	 * @param	string	the table name
	 * @param	array	the insert keys
	 * @param	array	the insert values
	 * @return	string
	 */
	function _insert($table, $keys, $values)
	{
		return "INSERT INTO ".$table." (".implode(', ', $keys).") VALUES (".implode(', ', $values).")";
	}

	// --------------------------------------------------------------------

	/**
	 * Update statement
	 *
	 * Generates a platform-specific update string from the supplied data
	 *
	 * @access	public
	 * @param	string	the table name
	 * @param	array	the update data
	 * @param	array	the where clause
	 * @param	array	the orderby clause
	 * @param	array	the limit clause
	 * @return	string
	 */
	function _update($table, $values, $where, $orderby = array(), $limit = FALSE)
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
	 * @access	public
	 * @param	string	the table name
	 * @return	string
	 */
	function _truncate($table)
	{
		return "TRUNCATE ".$table;
	}

	// --------------------------------------------------------------------

	/**
	 * Delete statement
	 *
	 * Generates a platform-specific delete string from the supplied data
	 *
	 * @access	public
	 * @param	string	the table name
	 * @param	array	the where clause
	 * @param	string	the limit clause
	 * @return	string
	 */
	function _delete($table, $where = array(), $like = array(), $limit = FALSE)
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
	 * @access	public
	 * @param	string	the sql query string
	 * @param	integer	the number of rows to limit the query to
	 * @param	integer	the offset value
	 * @return	string
	 */
	function _limit($sql, $limit, $offset)
	{
		$i = $limit + $offset;

		return preg_replace('/(^\SELECT (DISTINCT)?)/i','\\1 TOP '.$i.' ', $sql);
	}

	// --------------------------------------------------------------------

	/**
	 * Close DB Connection
	 *
	 * @access	public
	 * @param	resource
	 * @return	void
	 */
	function _close($conn_id)
	{
		@mssql_close($conn_id);
	}

}



/* End of file mssql_driver.php */
/* Location: ./system/database/drivers/mssql/mssql_driver.php */