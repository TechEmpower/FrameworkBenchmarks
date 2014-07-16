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
class DataDriverMySQL_PDO implements IDataDriver
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
		return "MySQL";
	}
	
	function Ping($connection)
	{
		 return mysql_ping($connection);
	}
	
	/**
	 * @inheritdocs
	 */
	function Open($connectionstring,$database,$username,$password,$charset='',$bootstrap='') 
	{
		if (!class_exists("PDO")) throw new DatabaseException('PDO extension is not enabled on this server.',DatabaseException::$CONNECTION_ERROR);
		
		$connection = null;
		
		try 
		{
			// if the port is provided in the connection string then strip it out and provide it as a separate param
			$hostAndPort = explode(":",$connectionstring);
			$host = $hostAndPort[0];
			$port = count($hostAndPort) > 1 ? $hostAndPort[1] : null;
			
			$dsn = 'mysql:dbname='. $this->Escape($database)  .';host=' . $this->Escape($host);
			
			if ($port) {
				$dsn .= ";port=" . $this->Escape($port);
			}
			
			if ($charset) {
				$dsn .= ";charset=" . $this->Escape($charset);
			}
			
			$connection = new PDO($dsn, $username, $password);
		} 
		catch (Exception $e) 
		{
			throw new DatabaseException("Error connecting to database: " . $e->getMessage(),DatabaseException::$CONNECTION_ERROR);
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
					throw new DatabaseException("Connection Bootstrap Error: " . $ex->getMessage(),DatabaseException::$ERROR_IN_QUERY);
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
		$connection = null; // ignore warnings
	}
	
	/**
	 * @inheritdocs
	 */
	function Query($connection,$sql) 
	{
		
		if (!$stmt = $connection->query($sql)) {
			throw new DatabaseException($this->GetErrorDescription($connection),DatabaseException::$ERROR_IN_QUERY);
		}
		
		return $stmt;
	}
	
	/**
	 * @inheritdocs
	 */
	function Execute($connection,$sql) 
	{
		$stmt = $connection->prepare($sql);
		
		if (!$stmt) {
			throw new DatabaseException($this->GetErrorDescription($connection),DatabaseException::$ERROR_IN_QUERY);
		}
		
		if (!$numRows = $stmt->execute())
		{
			throw new DatabaseException($this->GetErrorDescription($sth),DatabaseException::$ERROR_IN_QUERY);
		}
		
		return $numRows;
	}

	/**
	 * Given a PDO object, return the last error
	 * @param PDO:errorInfo $errorInfo
	 */
	private function GetErrorDescription($obj)
	{
		$errorInfo = $obj->errorInfo();
		return $errorInfo[2];
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
	function Fetch($connection,$rs) 
	{
		return $rs->fetch(PDO::FETCH_ASSOC);
	}

	/**
	 * @inheritdocs
	 */
	function GetLastInsertId($connection) 
	{
		return $connection->lastInsertId(); 
	}

	/**
	 * @inheritdocs
	 */
	function GetLastError($connection)
	{
		return $this->GetErrorDescription($connection);
	}
	
	/**
	 * @inheritdocs
	 */
	function Release($connection,$rs) 
	{
		$rs = null;
	}
	
	/**
	 * @inheritdocs
	 * this method currently uses replacement and not mysql_real_escape_string
	 * so that a database connection is not necessary in order to escape.
	 * this way cached queries can be used without connecting to the DB server
	 */
	function Escape($val) 
	{
		return str_replace(self::$BAD_CHARS, self::$GOOD_CHARS, $val);
		// return mysql_real_escape_string($val);
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
		$connection->beginTransaction();
	}
	
	/**
	 * @inheritdocs
	 */
	function CommitTransaction($connection)
	{
		$connection->commit();
	}
	
	/**
	 * @inheritdocs
	 */
	function RollbackTransaction($connection)
	{
		$connection->rollBack();
	}
}

?>