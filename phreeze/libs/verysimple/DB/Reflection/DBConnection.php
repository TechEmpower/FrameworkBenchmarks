<?php
/** @package    verysimple::DB::Reflection */

/** import supporting libraries */
require_once("DBEventHandler.php");
require_once("DBConnectionString.php");
require_once('verysimple/DB/DatabaseException.php');
require_once('verysimple/Phreeze/DataAdapter.php');

/**
 * DBConnection provides connectivity to a MySQL Server
 *
 * @package    verysimple::DB::Reflection
 * @author Jason Hinkle
 * @copyright  1997-2007 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version 1.0
 */
class DBConnection
{
	public $Host;
	public $Port;
	public $Username;
	public $Password;
	public $DBName;

	private $dbconn;
	private $csetting;
	private $adapter;
	private $handler;
	private $dbopen;

	/**
	 * Instantiate new DBConnection
	 *
	 * @access public
	 * @param DBConnectionString $host
	 * @param DBEventHandler $port
	 */
	function __construct($dbconnstring, $handler = null)
	{

        $this->dbopen = false;
        
		$this->Host = $dbconnstring->Host;
		$this->Port = $dbconnstring->Port;
		$this->Username = $dbconnstring->Username;
		$this->Password = $dbconnstring->Password;
		$this->DBName = $dbconnstring->DBName;
		
		// TODO: this is redundant after switching to the DataAdapter
		$this->csetting = new ConnectionSetting();
		$this->csetting->ConnectionString = $dbconnstring->Host . ($dbconnstring->Port ? ':' . $dbconnstring->Port : '');
		$this->csetting->DBName = $dbconnstring->DBName;
		$this->csetting->Username = $dbconnstring->Username;
		$this->csetting->Password = $dbconnstring->Password;
		$this->csetting->Type = $dbconnstring->Type;

		if ($handler)
		{
			$this->handler =& $handler;
		}
		else
		{
			$this->handler = new DBEventHandler();
		}

		$this->handler->Log(DBH_LOG_INFO, "Connection Initialized");
	}

    /**
     * Destructor closes the db connection.
     *
     * @access     public
     */
	function __destruct()
    {
        $this->Disconnect();
    }

    /**
	 * Opens a connection to the MySQL Server and selects the specified database
	 *
	 * @access public
	 * @param string $dbname
	 */
	function Connect()
	{
		$this->handler->Log(DBH_LOG_INFO, "Opening Connection...");
		if ($this->dbopen)
		{
			$this->handler->Log(DBH_LOG_WARNING, "Connection Already Open");
		}
		else
		{
			$this->adapter = new DataAdapter($this->csetting);
			
			try {
				$this->adapter->Open();
			}
			catch (Exception $ex) {
				$this->handler->Crash(DatabaseException::$CONNECTION_ERROR,$ex->getMessage());
			}

			$this->handler->Log(DBH_LOG_INFO, "Connection Open");
			$this->dbopen = true;
		}
	}

    /**
	 * Checks that the connection is open and if not, crashes
	 *
	 * @access public
	 * @param bool $auto Automatically try to connect if connection isn't already open
	 */
	private function RequireConnection($auto = false)
	{
		if (!$this->dbopen)
		{
			if ($auto)
			{
				$this->Connect();
			}
			else
			{
				$this->handler->Crash(DatabaseException::$CONNECTION_ERROR, "DB is not connected.  Please call DBConnection->Connect() first.");
			}
		}
	}

	/**
	 * Closing the connection to the MySQL Server
	 *
	 * @access public
	 */
	function Disconnect()
	{
		$this->handler->Log(DBH_LOG_INFO, "Closing Connection...");

		if ($this->dbopen)
		{
			$this->adapter->Close();
			$this->dbopen = false;
			$this->handler->Log(DBH_LOG_INFO, "Connection closed");
		}
		else
		{
			$this->handler->Log(DBH_LOG_WARNING, "Connection Already Closed");
		}
	}

	/**
	 * Executes a SQL select statement and returns a MySQL resultset
	 *
	 * @access public
	 * @param string $sql
	 * @return mysql_query
	 */
	function Select($sql)
	{
		$this->RequireConnection(true);

		$this->handler->Log(DBH_LOG_QUERY, "Executing Query", $sql);
		
		return $this->adapter->Select($sql);
	}

	/**
	 * Executes a SQL query that does not return a resultset
	 *
	 * @access public
	 * @param string $sql
	 */
	function Update($sql)
	{
		$this->RequireConnection(true);
		
		return $this->adapter->Escape($sql);
	}

	/**
	 * Moves the database curser forward and returns the current row as an associative array
	 *
	 * @access public
	 * @param mysql_query $rs
	 * @return Array
	 */
	function Next($rs)
	{
		$this->RequireConnection();
		
		$this->handler->Log(DBH_LOG_DEBUG, "Fetching next result as array");
		return $this->adapter->Fetch($rs);

	}

	/**
	 * Releases the resources for the given resultset
	 *
	 * @access public
	 * @param mysql_query $rs
	 */
	function Release($rs)
	{
		$this->RequireConnection();

		$this->handler->Log(DBH_LOG_DEBUG, "Releasing result resources");
		return $this->adapter->Release($rs);
	}

}

?>