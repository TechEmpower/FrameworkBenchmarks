<?php
/** @package    verysimple::Phreeze */

require_once("verysimple/HTTP/RequestUtil.php");
require_once("verysimple/Phreeze/IRouter.php");

/**
 * SimpleRouter is a URL router that parses URLs in the following format:
 * http://server/index.php?ROUTE
 * This router can be used in a situation where URL re-writing is not 
 * available or wanted on the host server
 * 
 * The route must be the first param in the querysting and does not
 * have a key.  Additional querystring parameters can be appended
 * and will have no effect on the route.
 * 
 * Note that this router does not currently support different routes
 * based on params passed through nor is wildcard matching supported
 *
 * @package    verysimple::HTTP
 * @author     VerySimple Inc.
 * @copyright  1997-2007 VerySimple, Inc. http://www.verysimple.com
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    1.0
 */
class SimpleRouter implements IRouter
{
	public static $ROUTE_NOT_FOUND = "Default.Error404";
	
	private $routes = array();
	private $defaultAction = 'Default.Home';
	private $appRootUrl = '';
	
	/**
	 * 
	 * @param string $appRootUrl
	 * @param string $defaultAction
	 * @param array $mapping routeMap
	 */
	public function __construct($appRootUrl = '', $defaultAction = '', $mapping = array())
	{
		if ($defaultAction) $this->defaultAction = $defaultAction;
		$this->routes = $mapping;
	}
	
	/**
	 * Given a controller, method and params, returns a url that points
	 * to the correct location
	 *
	 * @param string $controller
	 * @param string $method
	 * @param array $params in the format param1=val1&param2=val2
	 * @return string URL
	 */
	public function GetUrl($controller, $method, $params = '', $requestMethod = '')
	{
		throw new Exception('Not yet implemented');
	}
	
	/**
	 * Returns the controller and method for the given URI
	 *
	 * @param string the url, if not provided will be obtained using the current URL
	 * @return array($controller,$method)
	 */
	public function GetRoute($uri = "")
	{
		$match = '';
		
		$qs = $uri ? $uri : (array_key_exists('QUERY_STRING', $_SERVER) ? $_SERVER['QUERY_STRING'] : '');
		$parsed = explode('&', $qs,2);
		$action = $parsed[0];
		
		if (strpos($action, '=') > -1 || !$action) {
			// the querystring is empty or the first param is a named param, which we ignore
			$match = $this->defaultAction;
		}
		else {
			// otherwise we have a route.  see if we have a match in the routemap, otherwise return the 'not found' route
			$method = RequestUtil::GetMethod();
			$route = $method.':'.$action;
			$match = array_key_exists($route, $this->routes) ? $this->routes[$route]['route'] : self::$ROUTE_NOT_FOUND;
		}
		
		return explode('.',$match,2);
	}
	
	/**
	 * In the case of a rewrite url, the url itself contains the parameter
	 * for example http://server/param1/param2/param3.  These params
	 * are parsed and the param with the given index is returned
	 * @return string (or $default if not provided)
	 * @param string default value to return if parameter is empty
	 */
	public function GetUrlParam($key, $default = '')
	{
		return array_key_exists($key, $_REQUEST) ? $_REQUEST[$key] : $default;
	}
	
	/**
	 * In the case of a rewrite url, the url itself contains the parameter
	 * for example http://server/param1/param2/param3.  These params
	 * are parsed and returned as an array
	 * @return array
	 */
	public function GetUrlParams()
	{
		return $_REQUEST;
	}
	
	/**
	 * Returns the RESTful part of the url
	 * For example, localhost/users/5 will return users/5
	 * @return string
	 */
	public function GetUri()
	{
		
	}
	
}