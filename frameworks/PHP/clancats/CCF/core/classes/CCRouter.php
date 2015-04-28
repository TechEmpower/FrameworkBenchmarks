<?php namespace Core;
/**
 * Router
 * register and resolve routes that can be executed by an request
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCRouter 
{	
	/**
	 * The routes holder
	 *
	 * @var array
	 */
	protected static $routes = array();
	
	/**
	 * The alias holder
	 *
	 * @var array
	 */
	protected static $aliases = array();
	 
	/**
	 * the private routes holder
	 *
	 * @var array
	 */
	protected static $privates = array();
	
	/**
	 * The uri filters
	 *
	 * The initial filters here are used to escape special regex characters
	 * to allow them be used in the url. 
	 *
	 * @var array
	 */
	protected static $filters = array(
		'$' => '\$',
		'+' => '\+',
		'(' => '\(',
		')' => '\)'
	);
	
	/**
	 * The events
	 *
	 * @var array
	 */
	protected static $events = array();
	
	/**
	 * Set up the basic uri filters in our static init and 
	 * also add a default 404 response
	 *
	 * @return void
	 */
	public static function _init() 
	{	
		// default string
		static::filter( 'any', '[a-zA-Z0-9'.ClanCats::$config->get( 'router.allowed_special_chars' ).']' );
		
		// only numbers
		static::filter( 'num', '[0-9]' );
		
		// only alpha characters
		static::filter( 'alpha', '[a-zA-Z]' );
		
		// only alphanumeric characters
		static::filter( 'alphanum', '[a-zA-Z0-9]' );

		// 404 not found error
		CCRouter::on( '#404', function() 
		{
			return CCResponse::create( CCView::create( 'Core::CCF/404' )->render(), 404 );
		});
	}
	
	/**
	 * Creates an alias to a route or gets one
	 *
	 * @param string			$key
	 * @param string 		$to 
	 * @return void | false
	 */
	public static function alias( $key, $to = null )
	{
		if ( is_null( $to ) || is_array( $to ) ) 
		{
			if ( array_key_exists( $key, static::$aliases ) )
			{
				if ( is_array( $to ) )
				{	
					// Workaround for HHVM: return preg_replace( "/\[\w+\]/e", 'array_shift($to)', static::$aliases[$key] );
					$return = static::$aliases[$key];
					
					foreach( $to as $rpl )
					{
						$return = preg_replace( "/\[\w+\]/", $rpl, $return, 1 );
					}
					
					return $return;
				}
				return static::$aliases[$key];
			}
			return false;
		}
		static::$aliases[$key] = $to;
	}
	
	/**
	 * Register an event over a route
	 *
	 * @param string		$event
	 * @param string 	$route 
	 * @param mixed 		$callback
	 * @return void
	 */
	public static function event( $event, $route, $callback )
	{
		static::$events[$event][$route][] = $callback;
	}
	
	/**
	 * Get all events matching a rule
	 *
	 * @param string		$event
	 * @param string 	$route 
	 * @param mixed 		$callback
	 * @return void
	 */
	public static function events_matching( $event, $rule )
	{
		if ( !array_key_exists( $event, static::$events ) )
		{
			return array();
		}
		
		$callbacks = array();
		
		foreach( static::$events[$event] as $route => $events )
		{
			$rgx = "~^".str_replace( '*', '(.*)', $route )."$~";
			if ( preg_match( $rgx, $rule ) )
			{
				$callbacks = array_merge( $callbacks, $events );
			}
		}
		
		return $callbacks;
	}
	
	/**
	 * Add a route to the router
	 *
	 * example: 
	 *     CCRouter::on( 'user/mario', function(){} )
	 *     CCRouter::on( 'user/mario', array( 'alias' => 'profile' ), function(){} )
	 * 
	 * @param mixed		$param1
	 * @param mixed		$param2
	 * @param mixed		$param3
	 * @return void
	 */
	public static function on( $param1, $param2 = null, $param3 = null ) 
	{	
		// call contains options
		if ( !is_null( $param3 ) && !is_callable( $param2 ) ) 
		{
			// if the param2 is a string we going to use it as alias
			if ( is_string( $param2 ) ) 
			{
				static::alias( $param2, $param1 );
			}
			elseif ( is_array( $param2 ) ) 
			{
				// assign alias
				if ( array_key_exists( 'alias', $param2 ) ) 
				{
					static::alias( $param2['alias'], $param1 );
				}
				// asign wake event
				if ( array_key_exists( 'wake', $param2 ) ) 
				{
					static::event( 'wake', $param1, $param2['wake'] );
				}
				// assign sleep event
				if ( array_key_exists( 'sleep', $param2 ) ) 
				{
					static::event( 'sleep', $param1, $param2['sleep'] );
				}
			}
			
			$param2 = $param3; unset( $param3 );
		}
		
		if ( !is_array( $param1 ) ) 
		{
			$param1 = array( $param1 => $param2 );
		}
		
		static::prepare( $param1 );
	}
	
	/**
	 * Prepare the routes assing them to their containers
	 * 
	 * @param array		$routes
	 * @return void
	 */
	protected static function prepare( $routes ) 
	{	
		// flatten if needed
		if ( ClanCats::$config->get( 'router.flatten_routes' ) ) 
		{
			$routes = static::flatten( $routes ); 
		}
		
		foreach( $routes as $uri => $route ) 
		{	
			// check for an alias to a private
			if ( is_string( $route ) ) 
			{
				if ( substr( $route, 0, 1 ) == '#' ) 
				{
					$route = static::$privates[$route];
				}
			}
			
			// does this uri contain an alias?
			if ( strpos( $uri, '@' ) !== false )
			{
				// is the entire route an alias
				if ( $uri[0] == '@' )
				{
					static::$aliases[substr( $uri, 1 )] = $route;
				}
				// does it just contain an alias
				else
				{
					list( $uri, $alias ) = explode( '@', $uri );
					static::$aliases[$alias] = $uri;
				}
			}
			
			// check for a private 
			if ( substr( $uri, 0, 1 ) == '#' ) 
			{
				static::$privates[$uri] = $route;
			}
			// just a normal one
			else 
			{
				static::$routes[$uri] = $route;
			}
		}
	}
	
	/**
	 * Add an uri filter to the router
	 * 
	 * @param string		$key
	 * @param string		$pattern 	an regex pattern that should match the given parameter
	 * @return void
	 */
	public static function filter( $key, $pattern ) 
	{
		static::$filters['['.$key.']'] = '('.$pattern.'+)';
	}
	
	/**
	 * Flatten the routes
	 * 
	 * @param array		$routes
	 * @param string		$param_prefix
	 * @return array
	 */
	protected static function flatten( $routes, $param_prefix = '' ) 
	{
		
		$flattened = array();
		
		foreach( $routes as $prefix => $route ) 
		{
			if ( is_array( $route ) && !is_callable( $route ) ) 
			{
				$flattened = array_merge( static::flatten( $route, $param_prefix.$prefix.'/' ), $flattened );
			} 
			else 
			{
				if ( $prefix == '/' ) 
				{
					$prefix = ''; $param_prefix = substr( $param_prefix, 0, -1 );
					$flattened[$param_prefix.$prefix] = $route;
					$param_prefix.= '/';
				} 
				else 
				{
					$flattened[$param_prefix.$prefix] = $route;
				}
			}
		}
		
		return $flattened;
	}
	
	/**
	 * Resolve an uri and get the route object
	 *
	 * @param string 		$uri
	 * @return CCRoute
	 */
	public static function resolve( $uri ) 
	{	
		// cut unnecessary slashes and params
		if ( substr( $uri, -1 ) == '/' ) 
		{
			$uri = substr( $uri, 0, -1 );
		}
		
		if ( substr( $uri, 0, 1 ) == '/' ) 
		{
			$uri = substr( $uri, 1 );
		}
		
		$uri = CCStr::cut( $uri, '?' );
		
		// create new route instance
		$route = new CCRoute( $uri );
		
		// private route
		if ( substr( $uri, 0, 1 ) == '#' ) {
			if ( !array_key_exists( $uri, static::$privates ) ) 
			{
				throw new CCException( "CCRouter::resolve() - private route {$uri} could not be found." );
			}
			
			return static::configure( $route, static::$privates[$uri] );
		}
		
		// pass the route trough the priority events
		if ( $hook_route = CCEvent::pass( 'ccf.router.resolve.before', array( $route, false ) ) ) 
		{
			if ( $hook_route[1] === true ) 
			{
				return static::configure( $route, $hook_route[0] );
			}
		}
		
		// if the uri is empty root is requestet
		if ( empty( $uri ) ) 
		{
			// try one slash also aka empty
			if ( array_key_exists( '', static::$routes ) ) 
			{
				return static::configure( $route, static::$routes[''] );	
			}
			return static::configure( $route, static::$privates['#root'] );
		}
		
		// simple static key route
		if ( isset( static::$routes[$uri] ) ) 
		{
			return static::configure( $route, static::$routes[$uri] );
		}
		
		// explode the uri
		$uri_parts = explode( '/', $uri );
		
		// get the action
		$action = array_pop( $uri_parts );
			
		// implode the uri without action
		$uri_without_action = implode( '/', $uri_parts );
		
		// try the static key with an action
		if ( isset( $action ) ) 
		{
			if ( isset( static::$routes[$uri_without_action] ) ) 
			{
				if ( CCController::has_action( static::$routes[$uri_without_action], $action ) )
				{
					$route->action = $action;	
					return static::configure( $route, static::$routes[$uri_without_action] );
				}
			}
		}
		
		// dynamic keys
		foreach( static::$routes as $pattern => $dynamic_route ) 
		{	
			// try the dynamic route only if it can be a pattern
			if ( strpos( $pattern, '[' ) !== false && strpos( $pattern, ']' ) !== false ) 
			{
				// build an reqular expression	
				$regx = '#^'.CCStr::replace( $pattern, static::$filters ).'$#i';

				// try to match the uri with regex
				if ( preg_match( $regx, $uri, $params ) ) 
				{	
					// remove the first param
					unset( $params[0] );
					
					$route->action = null;
					$route->params = $params;
					
					return static::configure( $route, $dynamic_route );
				}
				
				// try to match the uri without the action with the regex
				if ( is_string($dynamic_route) && preg_match( $regx, $uri_without_action, $params ) ) 
				{
					if ( CCController::has_action( $dynamic_route, $action ) ) 
					{
						// remove the first param
						unset( $params[0] );
					
						$route->action = $action;
						$route->params = $params;
					
						return static::configure( $route, $dynamic_route );	
					}
				}
			}
		}      
		
		/*
		 * pass to events
		 */
		if ( $hook_route = CCEvent::pass( 'ccf.router.resolve.after', array( $route, false ) ) ) {
			if ( $hook_route[1] === true ) {
				return static::configure( $route, $hook_route[0] );
			}
		}
		
		return false;
	}
	
	/**
	 * Check and complete a route
	 *
	 * @param CCRoute 			$route
	 * @param mixed				$raw_route
	 * @return false|CCRoute
	 */
	protected static function configure( $route, $raw_route ) 
	{	
		// deal with emptiness
		if ( is_null( $raw_route ) )
		{
			return false;	
		}
		// this might be a controller
		if ( is_string( $raw_route ) ) 
		{	
			// are there overwrite parameters?
			if ( strpos( $raw_route, '?' ) !== false ) 
			{
				$route->params = explode( ',', CCStr::suffix( $raw_route, '?' ) );
				$raw_route = CCStr::cut( $raw_route, '?' );
			}
			
			// is there a overwrite action?
			if ( strpos( $raw_route, '@' ) !== false ) 
			{
				$route->action = CCStr::suffix( $raw_route, '@' );
				$raw_route = CCStr::cut( $raw_route, '@' );
			}
			
			// try creating an controller instance
			$controller = CCController::create( $raw_route );
			
			// set the callback on controller execute
			$route->callback = array( $controller, 'execute' );
		}
		// is the route callable set it up
		elseif ( is_callable( $raw_route ) ) 
		{
			$route->callback = $raw_route;
		}
		
		return $route;
	}
}
