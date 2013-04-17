<?php
/** @package    verysimple::Phreeze */

/** import supporting libraries */
require_once('IRouter.php');
require_once('verysimple/HTTP/RequestUtil.php');

/**
 * Generic Router is an implementation of IRouter that uses patterns to connect
 * routes to a Controller/Method
 *
 * @package    verysimple::Phreeze
 * @author     VerySimple Inc.
 * @copyright  1997-2012 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    1.0
 */
class GenericRouter implements IRouter
{
	private static $routes = array();

	private $defaultAction = 'Default.Home';
	private $uri = '';
	private $appRootUrl = '';

	// cached route from last run:
	private $cachedRoute;

	/**
	 * Instantiate the GenericRouter
	 *
	 * @param string $appRootUrl the root url of the application including trailing slash (ex http://localhost/)
	 * @param string $defaultAction action to call if no route is provided (ex Default.DefaultAction)
	 * @param array $mapping the associative array of maps Example: <pre>
	 * 		$routes = array(
	 * 			"GET:" => array("route" => "Default.Home"),
	 * 			"GET:user/(:num)/packages" => array("route" => "Package.Query", "params" => array("userId" => 1)),
	 * 			"POST:user/(:num)/package/(:num)" => array("route" => "Package.Update", "params" => array("userId" => 1, "packageId" => 3)),
	 * 			"GET:automation/(:any)" => array("route" => "Automation.DoAutomation", "params" => array("action" => 1))
	 * 		);
	 * </pre>
	 */
	public function __construct($appRootUrl = '', $defaultAction = '', $mapping = array())
	{
		if ($defaultAction) $this->defaultAction = $defaultAction;
		if ($appRootUrl) $this->appRootUrl = $appRootUrl;

		$this->mapRoutes($mapping);

		$this->cachedRoute = null;
	}

	/**
	 * Return the root url for this application as provided at construction
	 */
	public function GetRootUrl()
	{
		return $this->appRootUrl;
	}

	/**
	 * Adds router mappings to our routes array.
	 *
	 * @param array $src
	 */
	private static function mapRoutes( $src )
	{
		foreach ( $src as $key => $val )
			self::$routes[ $key ] = $val;
	}

	/**
	 * @inheritdocs
	 */
	public function GetRoute( $uri = "" )
	{
		if( $uri == "" )
			$uri = RequestUtil::GetMethod() . ":" . $this->GetUri();

		// literal match check
		if ( isset(self::$routes[ $uri ]) )
		{
			// expects mapped values to be in the form: Controller.Model
			list($controller,$method) = explode(".",self::$routes[ $uri ]["route"]);

			$this->cachedRoute = array(
				"key" => self::$routes[ $uri ]
				,"route" => self::$routes[ $uri ]["route"]
				,"params" => isset(self::$routes[ $uri ]["params"]) ? self::$routes[ $uri ]["params"] : array()
			);

			return array($controller,$method);
		}

		// loop through the route map for wild cards:
		foreach( self::$routes as $key => $value)
		{
			$unalteredKey = $key;

			// convert wild cards to RegEx.
			// currently only ":any" and ":num" are supported wild cards
			$key = str_replace( ':any', '.+', $key );
			$key = str_replace( ':num', '[0-9]+', $key );

			// check for RegEx match
			if ( preg_match( '#^' . $key . '$#', $uri ) )
			{
				$this->cachedRoute = array(
					"key" => $unalteredKey
					,"route" => $value["route"]
					,"params" => isset($value["params"]) ? $value["params"] : array()
				);

				// expects mapped values to be in the form: Controller.Model
				list($controller,$method) = explode(".",$value["route"]);
				return array($controller,$method);
			}
		}

		// if we haven't returned by now, we've found no match:
		return array("Default","Error404");
	}

	/**
	 * @see IRouter::GetUri()
	 */
	public function GetUri()
	{
		if (!$this->uri)
		{
			$this->uri = array_key_exists('_REWRITE_COMMAND', $_REQUEST) ? $_REQUEST['_REWRITE_COMMAND'] : '';

			// if a root folder was provided, then we need to strip that out as well
			if ($this->appRootUrl)
			{
				$prefix = str_replace(RequestUtil::GetServerRootUrl(),'/',$this->appRootUrl);
				if (substr($this->uri,0,strlen($prefix)) == $prefix)
				{
					$this->uri = substr($this->uri,strlen($prefix));
				}
			}

			// strip trailing slash
			while (substr($this->uri,-1) == '/')
			{
				$this->uri = substr($this->uri,0,-1);
			}
		}
		return $this->uri;
	}

	/**
	 * @inheritdocs
	 */
	public function GetUrl( $controller, $method, $params = '', $requestMethod = "" )
	{
		$found = false;
		// $requestMethod = RequestUtil::GetMethod();

		if ($params == '') $params = array();
		
		if (!is_array($params))
		{
			$pairs = explode('&',$params);
			$params = array();
			foreach ($pairs as $pair)
			{
				$keyval = explode('=',$pair);
				$params[$keyval[0]] = count($keyval) > 1 ? $keyval[1] : '';
			}
		}

		// if an appRootUrl was provided then use that, otherwise figure it out based on the root url
		$url = $this->appRootUrl ? $this->appRootUrl : RequestUtil::GetBaseURL();

		// normalize url by stripping trailing slash
		while (substr($url,-1) == '/')
		{
			$url = substr($url,0,-1);
		}

		foreach( self::$routes as $key => $value)
		{
			list($routeController,$routeMethod) = explode(".",$value["route"]);

			$keyRequestMethodArr = explode(":",$key,2);
			$keyRequestMethod = $keyRequestMethodArr[0];

			// echo "$routeController:$controller $routeMethod:$method $keyRequestMethod:$requestMethod)<br/>";
			if( ($routeController == $controller) && ($routeMethod == $method) && ($requestMethod == "" || ($keyRequestMethod == $requestMethod)) &&
			    (! array_key_exists("params",$value) || count($params) == count($value["params"]) )
			  )
			{
				$keyArr = explode('/',$key);

				// strip the request method off the key:
				// we can safely access 0 here, as there has to be params to get here:
				$reqMethodAndController = explode(":",$keyArr[0]);
				$keyArr[0] = (count($reqMethodAndController) == 2 ? $reqMethodAndController[1] : $reqMethodAndController[0]);

				// merge the parameters passed in with the routemap's path
				// example: path is user/(:num)/events and parameters are [userCode]=>111
				// this would yield an array of [0]=>user, [1]=>111, [2]=>events
				if( array_key_exists("params",$value) )
				{
					foreach( $value["params"] as $rKey => $rVal )
					{
						if (!array_key_exists($rKey, $params))
							throw new Exception('Missing parameter "' . $rKey . "' for route " . $controller.'.'.$method);
						$keyArr[$value["params"][$rKey]] = $params[$rKey];
					}
				}

				// put the url together:
				foreach( $keyArr as $urlPiece )
				{
					$url = $url . ($urlPiece != '' ? "/$urlPiece" : '');
				}
				
				// no route, just a request method? RESTful to add a trailing slash:
				if( $keyRequestMethodArr[1] == "")
					$url = $url . "/";

				$found = true;
				break;
			}
		}

		// we stripped this at the beginning, need to add it back
		if( ! $found ) // $url = $url . "/";
			throw new Exception("No route found for " . $controller.'.'.$method. ($params ? '?' . implode('&',$params) : '' ));

		return $url;
	}

	/**
	 * @inheritdocs
	 */
	public function GetUrlParams()
	{
		return explode('/',$this->GetUri());
	}

	/**
	 * @inheritdocs
	 */
	public function GetUrlParam($paramKey, $default = '')
	{
		if( $this->cachedRoute == null )
			throw new Exception("Call GetRoute before accessing GetUrlParam");

		$params = $this->GetUrlParams();
		$uri = $this->GetUri();
		$count = 0;
		$routeMap = $this->cachedRoute["key"];

		if( isset($this->cachedRoute["params"][$paramKey]) )
		{
			$indexLocation = $this->cachedRoute["params"][$paramKey];
			return $params[$indexLocation];
		}
		else
			return RequestUtil::Get($paramKey,"");
	}
}
?>
