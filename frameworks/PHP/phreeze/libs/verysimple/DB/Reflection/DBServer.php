<?php
/** @package    verysimple::DB::Reflection */

/** import supporting libraries */
require_once("DBConnection.php");
require_once("DBSchema.php");

/**
 * DBServer is an object representation of a MySQL Server
 *
 * @package    verysimple::DB::Reflection
 * @author Jason Hinkle
 * @copyright  1997-2007 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version 1.0
 */
class DBServer
{
	public $Connection;
	public $SchemaName;
	
	/**
	 * Instantiate new DBServer
	 *
	 * @access public
	 * @param DBConnection $connection
	 */	
	function __construct($connection)
	{
		$this->Connection =& $connection;
	}
	

	/**
	 * Return the schema with the given name from this server
	 *
	 * @access public
	 * @param string $name
	 * @return DBSchema	
	 */	
	function GetSchema()
	{
		$this->Connection->Connect();
		
		$schema = new DBSchema($this);
		
		$this->Connection->Disconnect();
		
		return $schema;
	}

}

?>