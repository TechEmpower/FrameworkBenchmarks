<?php
/** @package    verysimple::HTTP */

/**
 * Context Persistance Storage
 * 
 * The context provides an object that can be uses globally for
 * dependency injection when passing information that should be 
 * available to an entire application
 *
 * @package    verysimple::HTTP 
 * @author     VerySimple Inc.
 * @copyright  1997-2007 VerySimple, Inc. http://www.verysimple.com
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    1.0
 */
class Context
{
	public $GUID;
	
	/**
	 * Constructor initializes the session
	 */
	public function __construct()
	{
		if (session_id() == '')
		{
			@session_start();
		}
	}
	
	/**
	 * Returns a persisted object or value
	 *
	 * @param var
	 * @param default value (default = null)
	 * @return value of var (or default)
	 */
	public function Get($var,$default = null)
	{
		return (isset($_SESSION[$this->GUID . "_" . $var])) ? unserialize($_SESSION[$this->GUID . "_" . $var]) : null;
	}

	/**
	 * Persists an object or value
	 *
	 * @access public
	 * @param var
	 * @param value
	 * @return object || null
	 */
	public function Set($var,$val)
	{
		$_SESSION[$this->GUID . "_" . $var] = serialize($val);
	}

}

?>