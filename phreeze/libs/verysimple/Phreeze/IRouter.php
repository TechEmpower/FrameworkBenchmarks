<?php
/** @package    verysimple::Phreeze */

/**
 * IRouter is an interface used for routing URLs to a Controller/Method
 *
 * @package    verysimple::Phreeze
 * @author     VerySimple Inc.
 * @copyright  1997-2012 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    1.0
 */
interface IRouter
{
	/**
	 * Given a controller, method and params, returns a url that points
	 * to the correct location
	 *
	 * @param string $controller
	 * @param string $method
	 * @param array $params in the format param1=val1&param2=val2
	 * @return string URL
	 */
	public function GetUrl($controller, $method, $params = '', $requestMethod = '');

	/**
	 * Returns the controller and method for the given URI
	 *
	 * @param string the url, if not provided will be obtained using the current URL
	 * @return array($controller,$method)
	 */
	public function GetRoute($uri = "");

	/**
	 * In the case of a rewrite url, the url itself contains the parameter
	 * for example http://server/param1/param2/param3.  These params
	 * are parsed and the param with the given index is returned
	 * @return string (or $default if not provided)
	 * @param string default value to return if parameter is empty
	 */
	public function GetUrlParam($key, $default = '');

	/**
	 * In the case of a rewrite url, the url itself contains the parameter
	 * for example http://server/param1/param2/param3.  These params
	 * are parsed and returned as an array
	 * @return array
	 */
	public function GetUrlParams();

	/**
	 * Returns the RESTful part of the url
	 * For example, localhost/users/5 will return users/5
	 * @return string
	 */
	public function GetUri();
}
?>
