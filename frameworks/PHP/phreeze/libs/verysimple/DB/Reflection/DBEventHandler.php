<?php
/** @package    verysimple::DB::Reflection */

/** import supporting libraries */

require_once('verysimple/DB/DatabaseException.php');

define("DBH_LOG_NONE",1);
define("DBH_LOG_INFO",2);
define("DBH_LOG_DEBUG",4);
define("DBH_LOG_QUERY",8);
define("DBH_LOG_WARNING",16);
define("DBH_LOG_ERROR",32);
 
/**
 * DBEventHandler is an optional parameter that can be used to hook into events in the 
 * DAO system for intercepting, debugging and observing
 *
 * @package    verysimple::DB::Reflection
 * @author Jason Hinkle
 * @copyright  1997-2007 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version 1.0
 */
 class DBEventHandler
{
	public $LogLevel;
	
	function __construct($level = DBH_LOG_NONE)
	{
		$this->LogLevel = $level;
	}
	
	/**
	 * Called by DB objects to report logging information
	 *
	 * @access public
	 * @param int $level
	 * @param string $message
	 * @param string $data
	 */	
	function Log($level,$message, $data = "")
	{
		$data = $data != "" ? ": $data" : "";
		switch ($level)
		{
			case DBH_LOG_DEBUG:
				if ($this->LogLevel & DBH_LOG_DEBUG) print "<pre style='color: silver;'>$message</pre>\r\n";
				break;
			case DBH_LOG_INFO:
				if ($this->LogLevel & DBH_LOG_INFO) print "<pre style='color: blue;'>$message $data</pre>\r\n";
				break;
			case DBH_LOG_QUERY:
				if ($this->LogLevel & DBH_LOG_QUERY) print "<pre style='color: green;'>$message $data</pre>\r\n";
				break;
			case DBH_LOG_WARNING:
				if ($this->LogLevel & DBH_LOG_WARNING) print "<pre style='color: orange;'>$message $data</pre>\r\n";
				break;
			case DBH_LOG_ERROR:
				if ($this->LogLevel & DBH_LOG_ERROR) print "<pre style='color: red;'>$message $data</pre>\r\n";
				break;
		}
		
	}
	
	/**
	 * Called by DB objects when a critical error occurs
	 *
	 * @access public
	 * @param int $code unique numerical identifier for error
	 * @param string $message human-readable error
	 * @param string $data any additional information that may help with debugging
	 */	
	function Crash($code, $message = "", $data = "")
	{
		throw new DatabaseException($message,$code,$data);
	}
}

?>