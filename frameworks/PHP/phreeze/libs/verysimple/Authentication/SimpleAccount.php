<?php
/** @package    verysimple::Authentication */

require_once("IAuthenticatable.php");

/**
 * simple implementation of IAuthenticatable for using a basic
 * hard-coded username/password combination.
 * @package    verysimple::Authentication
 * @author     VerySimple Inc.
 * @copyright  1997-2007 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    1.0
 */
class SimpleAccount implements IAuthenticatable
{
	private $_authenticated = false;
	private $_username;
	private $_password;
	
	public function __construct($required_username,$required_password)
	{
		$this->_username = $required_username;
		$this->_password = $required_password;
	}
	
	public function IsAnonymous()
	{
		return (!$this->_authenticated);
	}
		
	public function IsAuthorized($permission)
	{
		return $this->_authenticated;
	}
	
	public function Login($username,$password)
	{
		if ($this->_username == $username && $this->_password == $password)
		{
			$this->_authenticated = true;
			return true;
		}
		
		return false;
	}
	
	/**
	 * This is implemented only for Phreeze but is not utilized
	 * @param object
	 */
	public function Refresh($obj)
	{
		
	}
}

?>