<?php
/** @package    verysimple::Authentication */

/**
 * Classes implementing IAuthenticatable can be used with Authenticator for checking permissions
 * @package    verysimple::Authentication
 * @author     VerySimple Inc.
 * @copyright  1997-2007 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    1.0
 */
interface IAuthenticatable
{
	/**
	 * Return true if the user is anonymous, meaning they have not
	 * yet authenticated
	 * @return bool
	 */
	public function IsAnonymous();
	
	/**
	 * Verify if the IAuthenticable object has the requested permission
	 * and return either true or false
	 * @param variant $permission
	 * @return bool
	 */
	public function IsAuthorized($permission);
	
	/**
	 * Verify the login information and, if correct, this object will
	 * populate itself and return a reference to $this.  If login fails
	 * then return false/null.
	 * @param string $username
	 * @param string $password
	 * @return IAuthenticatable or false/null
	 */
	public function Login($username,$password);
}

?>