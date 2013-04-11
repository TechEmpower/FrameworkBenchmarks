<?php
/** @package    verysimple::DB::Reflection */

/**
 * DBConnectionString specifies the connection information
 *
 * @package    verysimple::DB::Reflection
 * @author Jason Hinkle
 * @copyright  1997-2007 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version 1.0
 */
 class DBConnectionString
{
	public $Host;
	public $Port;
	public $Username;
	public $Password;
	public $DBName;
	
	/**
	 * Create a new instance of a DBConnectionString
	 *
	 * @access public
	 * @param string $host
	 * @param string $port
	 * @param string $username
	 * @param string $password	 
	 * @param string $$dbname	 
	 */
	function __construct($host = "", $port = "", $username = "", $password = "", $dbname = "")
	{
		$this->Host = $host;
		$this->Port = $port;
		$this->Username = $username;
		$this->Password = $password;
		$this->DBName = $dbname;
	}
	
}

?>