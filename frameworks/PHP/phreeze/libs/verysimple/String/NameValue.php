<?php
/** @package    verysimple::String */

/**
 * A abstraction of NameValue pairs with static functions for parsing simple text structures
 *
 * @package    verysimple::String
 * @author Jason Hinkle
 * @copyright  1997-2008 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version 1.0
 */
class NameValue
{
	public $Code;
	public $Total;
	
	/**
	 * Constructor optionally accepts a line that will be parsed into a name/value
	 * @access public
	 * @param $line the line to be parsed
	 * @param $delim (default "=")
	 */
	function __construct($line = "", $delim = "=", $nameonly=false)
	{
		$keyval = explode($delim,$line);
		$this->Name = $keyval[0];
		$this->Value = $nameonly == false && isset($keyval[1]) ? $keyval[1] : $keyval[0];
	}
	
	/**
	 * Parses a string into an array of NameValue objects.
	 * @access public
	 * @param $lines string in the format name1=val1\nname2=val2 etc...
	 * @param $delim the delimiter between name and value (default "=")
	 * @param $nameonly returns only the name in the name/value pair
	 * @return Array of NameValue objects
	 */
	static function Parse($lines, $delim="=", $nameonly=false)
	{
		$return = array();
		
		$lines = str_replace("\r\n","\n",$lines);
		$lines = str_replace("\r","\n",$lines);
		$arr = explode("\n", $lines );
		
		if ($lines=="") return $return;
		
		foreach ($arr as $line)
		{
			$return[] = new NameValue($line,$delim,$nameonly);
		}
		
		return $return;
	}
	
	/**
	 * Converts an array of NameValue objects into a simple 1 dimensional array.
	 * WARNING: if there are duplicate Names in your array, they will be overwritten
	 * @access public
	 * @param $nvArray Array of NameValue objects (as returned from Parse)
	 * @return array
	 */
	static function ToSimpleArray($nvArray)
	{
		$sa = array();
		foreach ($nvArray as $nv)
		{
			$sa[$nv->Name] = $nv->Value;
		}
		return $sa;
	}
	
}

?>