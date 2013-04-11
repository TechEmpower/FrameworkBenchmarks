<?php
/** @package    verysimple::DB::Reflection */

/** import supporting libraries */
require_once("DBTable.php");

/**
 * DBSchema is an object representation of a MySQL Schema/Database
 *
 * @package    verysimple::DB::Reflection
 * @author Jason Hinkle
 * @copyright  1997-2007 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version 1.0
 */
class DBSchema
{
	public $Server;
	public $Name;
	public $Tables;
	
	/**
	 * Instantiate new DBSchema
	 *
	 * @access public
	 * @param DBServer $server
	 * @param string $name name of schema to parse	
	 */	
	function __construct($server)
	{
		$this->Server =& $server;
		$this->Name = $server->Connection->DBName;
		$this->Tables = Array();
		
		$this->Load();
		
		// print "<pre>"; print_r($this->Tables["ticket"]); die();
	}

	/**
	 * Inspects the current schema and loads all tables, keys, etc.
	 *
	 * @access public
	 */	
	private function Load()
	{
		$sql = "show tables";
		$rs = $this->Server->Connection->Select($sql);
		
		// first pass load all the tables.  this will initialize each object.  we have to
		// do this first so that we can correctly determine and store "Set" information
		while ($row = $this->Server->Connection->Next($rs))
		{
			$this->Tables[$row["Tables_in_" . $this->Name]] = new DBTable($this,$row);
		}
		
		// now load all the keys and constraints for each table
		foreach ($this->Tables as $table)
		{
			$table->LoadKeys();
		}

		$this->Server->Connection->Release($rs);
		
		$sql = "show table status from `" . $this->Name . "`";
		$rs2 = $this->Server->Connection->Select($sql);
		
		// load the extra data
		while ($row = $this->Server->Connection->Next($rs2))
		{
			$this->Tables[$row["Name"]]->Engine = $row["Engine"]; 
			$this->Tables[$row["Name"]]->Comment = $row["Comment"];
		}
		$this->Server->Connection->Release($rs2);
	}
}

?>