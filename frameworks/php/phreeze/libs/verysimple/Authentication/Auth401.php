<?php
/** @package    verysimple::Authentication */

/**
* Auth404 provided 401 authentication
* @package    verysimple::Authentication
* @author     VerySimple Inc.
* @copyright  1997-2007 VerySimple, Inc.
* @license    http://www.gnu.org/licenses/lgpl.html  LGPL
* @version    1.0
*/
class Auth401
{

	/**
	* Send 401 headers to the browser
	* @param string message to output as "Basic realm" text/message (default "Login Required")
	* @param bool true to terminate php after outputting headers (default true)
	*/
	static function OutputHeaders($realm = "Login Required", $terminate = true)
	{
		header("WWW-Authenticate: Basic realm=\"".$realm."\"");
		header("Status: 401 Unauthorized");
		header("HTTP-Status: 401 Unauthorized");
		if ($terminate) die();
	}
	
	/**
	 * Returns the server AUTH_USERNAME if provided or returns empty string
	 * @return string
	 */
	static function GetUsername()
	{
		return isset($_SERVER["PHP_AUTH_USER"]) ? $_SERVER["PHP_AUTH_USER"] : "";
	}

	/**
	 * Returns the server AUTH_PASSWORD if provided or returns empty string
	 * @return string
	 */
	static function GetPassword()
	{
		return isset($_SERVER["PHP_AUTH_PW"]) ? $_SERVER["PHP_AUTH_PW"] : "";
	}
}

?>