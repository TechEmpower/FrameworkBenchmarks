<?php 
/** @package verysimple::DB::DataDriver */

require_once("IDataDriver.php");
require_once("verysimple/DB/ISqlFunction.php");
require_once("verysimple/DB/DatabaseException.php");
require_once("verysimple/DB/DatabaseConfig.php");

/**
 * An implementation of IDataDriver that communicates with
 * a MySQL server.  This is one of the native drivers
 * supported by Phreeze
 *
 * @package    verysimple::DB::DataDriver
 * @author     VerySimple Inc. <noreply@verysimple.com>
 * @copyright  1997-2010 VerySimple Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    1.0
 */
class DataDriverMySQLi implements IDataDriver
{	
	/** @var characters that will be escaped */
	static $BAD_CHARS = array("\\","\0","\n","\r","\x1a","'",'"');
	
	/** @var characters that will be used to replace bad chars */
	static $GOOD_CHARS = array("\\\\","\\0","\\n","\\r","\Z","\'",'\"');
	
	/**
	 * @inheritdocs
	 */
	function GetServerType()
	{
		return "MySQLi";
	}
	
	function Ping($connection)
	{
		 return mysqli_ping($connection);
	}
	
	/**
	 * @inheritdocs
	 */
	function Open($connectionstring,$database,$username,$password,$charset='',$bootstrap='') 
	{
		if (!function_exists("mysqli_connect")) throw new DatabaseException('mysqli extension is not enabled on this server.',DatabaseException::$CONNECTION_ERROR);
		
		// if the port is provided in the connection string then strip it out and provide it as a separate param
		$hostAndPort = explode(":",$connectionstring);
		$host = $hostAndPort[0];
		$port = count($hostAndPort) > 1 ? $hostAndPort[1] : null;
		
		$connection = mysqli_connect($host, $username, $password, $database, $port);
		
		if ( mysqli_connect_errno() )
		{
			throw new DatabaseException("Error connecting to database: " . mysqli_connect_error(),DatabaseException::$CONNECTION_ERROR);
		}
		
		if ($charset)
		{
			mysqli_set_charset($connection,$charset);
			
			if ( mysqli_connect_errno() )
			{
				throw new DatabaseException("Unable to set charset: " . mysqli_connect_error(),DatabaseException::$CONNECTION_ERROR);
			}
		}
		
		if ($bootstrap)
		{
			$statements = explode(';',$bootstrap);
			foreach ($statements as $sql)
			{
				try 
				{
					$this->Execute($connection, $sql);
				}
				catch (Exception $ex)
				{
					throw new DatabaseException("problem with bootstrap sql: " . $ex->getMessage(),DatabaseException::$ERROR_IN_QUERY);
				}
			}
		}
		
		return $connection;
	}
	
	/**
	 * @inheritdocs
	 */
	function Close($connection) 
	{
		@mysqli_close($connection); // ignore warnings
	}
	
	/**
	 * @inheritdocs
	 */
	function Query($connection,$sql) 
	{
		if ( !$rs = @mysqli_query($connection,$sql) )
		{
			throw new DatabaseException(mysqli_error($connection),DatabaseException::$ERROR_IN_QUERY);
		}
		
		return $rs;
	}

	/**
	 * @inheritdocs
	 */
	function Execute($connection,$sql) 
	{
		if ( !$result = @mysqli_query($connection,$sql) )
		{
			throw new DatabaseException(mysqli_error($connection),DatabaseException::$ERROR_IN_QUERY);
		}
		
		return mysqli_affected_rows($connection);
	}
	
	/**
	 * @inheritdocs
	 */
	function Fetch($connection,$rs) 
	{
		return mysqli_fetch_assoc($rs);
	}

	/**
	 * @inheritdocs
	 */
	function GetLastInsertId($connection) 
	{
		return (mysqli_insert_id($connection));
	}

	/**
	 * @inheritdocs
	 */
	function GetLastError($connection)
	{
		return mysqli_error($connection);
	}
	
	/**
	 * @inheritdocs
	 */
	function Release($connection,$rs) 
	{
		mysqli_free_result($rs);	
	}
	
	/**
	 * @inheritdocs
	 * this method currently uses replacement and not mysqli_real_escape_string
	 * so that a database connection is not necessary in order to escape.
	 * this way cached queries can be used without connecting to the DB server
	 */
	function Escape($val) 
	{
		return str_replace(self::$BAD_CHARS, self::$GOOD_CHARS, $val);
		// return mysqli_real_escape_string($val);
 	}

 	/**
 	 * @inheritdocs
 	 */
 	public function GetQuotedSql($val)
 	{
 		if ($val === null) return DatabaseConfig::$CONVERT_NULL_TO_EMPTYSTRING ? "''" : 'NULL';
 	
 		if ($val instanceof ISqlFunction) return $val->GetQuotedSql($this);
 	
 		return "'" . $this->Escape($val) . "'";
 	}
 	
	/**
	 * @inheritdocs
	 */
 	function GetTableNames($connection, $dbname, $ommitEmptyTables = false) 
	{
		$sql = "SHOW TABLE STATUS FROM `" . $this->Escape($dbname) . "`";
		$rs = $this->Query($connection,$sql);
		
		$tables = array();
		
		while ( $row = $this->Fetch($connection,$rs) )
		{
			if ( $ommitEmptyTables == false || $rs['Data_free'] > 0 )
			{
				$tables[] = $row['Name'];
			}
		}
		
		return $tables;
 	}
	
	/**
	 * @inheritdocs
	 */
 	function Optimize($connection,$table) 
	{
		$result = "";
		$rs = $this->Query($connection,"optimize table `". $this->Escape($table)."`");

		while ( $row = $this->Fetch($connection,$rs) )
		{
			$tbl = $row['Table'];
			if (!isset($results[$tbl])) $results[$tbl] = "";
			$result .= trim($results[$tbl] . " " . $row['Msg_type'] . "=\"" . $row['Msg_text'] . "\"");	
		}
		
		return $result;
	}

	/**
	 * @inheritdocs
	 */
	function StartTransaction($connection)
	{
		$this->Execute($connection, "SET AUTOCOMMIT=0");
		$this->Execute($connection, "START TRANSACTION");
	}
	
	/**
	 * @inheritdocs
	 */
	function CommitTransaction($connection)
	{
		$this->Execute($connection, "COMMIT");
		$this->Execute($connection, "SET AUTOCOMMIT=1");
	}
	
	/**
	 * @inheritdocs
	 */
	function RollbackTransaction($connection)
	{
		$this->Execute($connection, "ROLLBACK");
		$this->Execute($connection, "SET AUTOCOMMIT=1");
	}
	
}

?>