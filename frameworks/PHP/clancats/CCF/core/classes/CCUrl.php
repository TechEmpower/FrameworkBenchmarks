<?php namespace Core;
/**
 * ClanCats Url
 * @todo if have to rewrite this shitty class some day..
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCUrl 
{
	/**
	 * The configured path offset
	 *
	 * @var string
	 */
	private static $path_offset = null;

	/**
	 * Parameter provider can add predifned get
	 *
	 * @var array
	 */
	private static $parameter_provider = array();

	/**
	 * static CCUrl initialisation
	 */
	public static function _init() 
	{
		static::$path_offset = ClanCats::$config->get( 'url.path', '/' );

		if ( empty( static::$path_offset ) )
		{
			static::$path_offset = '/';
		}

		if ( substr( static::$path_offset, -1 ) != '/' )
		{
			static::$path_offset .= '/';
		}

		// register the default parameter providers
		static::$parameter_provider = array(

			// Adds a session fingerprint to the parameter list
			'fingerprint' => function()
			{
				return array( ClanCats::$config->get( 'session.default_fingerprint_parameter' ) => fingerprint() );
			},

			// Adds the next redirect uri to redirect back on request
			'back' => function()
			{
				$params = CCIn::$_instance->GET; unset( $params['next'] );
				return array( 'next' => CCUrl::current( $params ) );
			},
		);
	}

	/**
	 * Register a parameter provider
	 *
	 * @param string			$key
	 * @param callback		$callback
	 * @return void
	 */
	public static function param( $key, $callback )
	{
		static::$parameter_provider[$key] = $callback;
	}

	/**
	 * Generate an url
	 *
	 * @param string			$uri
	 * @param array			$params
	 * @param bool			$retain		Should we keep the get parameters?
	 * @return string 
	 */
	public static function to( $uri = '', $params = array(), $retain = false ) 
	{
		// To avoid // urls we check for a single slash.
		if ( $uri === '/' ) 
		{
			$uri = '';
		}

		// When the uri starts with an @ sign we handle the uri as route alias.
		if ( substr( $uri, 0, 1 ) == '@' ) 
		{
			return static::alias( substr( $uri, 1 ), $params, $retain );
		}

		// Are there already parameters in the uri? Parse them
		// and merge them with current argument parameters
		if ( strpos( $uri, '?' ) !== false )
		{
			$parts = explode( '?', $uri );

			$uri = $parts[0];

			if ( isset( $parts[1] ) )
			{
				parse_str( $parts[1], $old_params );

				$params = array_merge( $old_params, $params );
			}
		}


		// When the uri contains a protocoll or starts with a slash we assume
		// a full url is given and we don't have to add a path offest.
		if ( strpos( $uri, '://' ) === false && substr( $uri, 0, 1 ) !== '/' ) 
		{
			$uri = static::$path_offset.$uri;
		}


		// Try to replace parameters in the uri and remove them from
		// the array so we can append them as get parameters
		foreach( $params as $key => $value )
		{
			// replace the parameter provider
			if ( is_numeric( $key ) && is_string($value) && $value[0] === ':' )
			{
				$param_provider_key = substr( $value, 1 );

				// remove the parameter provider from the parameter list
				unset( $params[$key] );

				if ( array_key_exists( $param_provider_key, static::$parameter_provider ) )
				{
					$params = $params + call_user_func( static::$parameter_provider[$param_provider_key] );
				}
			}

			// replace the parameters
			if ( !is_array($value))
			{
				$uri = str_replace( ':'.$key, $value, $uri, $count );

				if ( $count > 0 )
				{
					unset( $params[$key] );
				}
			}
		}

		// Should we keep the get parameters? If retain is enabled
		// we merge the get parameter array with argument parameters
		if ( $retain )
		{
			$params = array_merge( CCIn::$_instance->GET, $params );
		}

		// When we still got parameters add them to the url
		if ( !empty( $params ) ) 
		{
			$uri .= '?'.http_build_query( $params );
		}

		return $uri;
	}

	/**
	 * Create an URL based on an router alias
	 *
	 * @param string		$alias
	 * @param array  	$params
	 * @param bool		$retain		Should we keep the get parameters?
	 * @return string 
	 */
	public static function alias( $alias, $params = array(), $retain = false )
	{
		$route_params = array();
		
		// to handle the suffix after a slash in an alias define
		$suffix = '';
		
		if ( strpos( $alias, '/' ) !== false && $alias !== '/' )
		{
			// slashes in aliases get appended as suffix
			list( $alias, $suffix ) = explode( '/', $alias );
			
			$suffix = '/'.$suffix;
		}
	

		// get the parameters with the numeric keys so we can 
		// pass them as route parameters like [any]-[num]-[num]/[any]/
		foreach( $params as $key => $value )
		{
			if ( is_int( $key ) )
			{
				$route_params[] = $value; unset( $params[$key] );
			}
		}

		return CCUrl::to( CCRouter::alias( $alias, $route_params ).$suffix, $params, $retain );
	}

	/**
	 * Create the full url including protocol and hostname
	 *
	 * @param string		$uri
	 * @param array  	$params
	 * @param bool		$retain		Should we keep the get parameters?
	 * @return string 
	 */
	public static function full( $uri = '', $params = array(), $retain = false ) 
	{
		return CCIn::protocol().'://'.CCIn::host().static::to( $uri, $params, $retain );
	}

	/**
	 * Create the url and force the https protocol
	 *
	 * @param string		$uri
	 * @param array  	$params
	 * @param bool		$retain		Should we keep the get parameters?
	 * @return string 
	 */
	public static function secure( $uri = '', $params = array(), $retain = false ) 
	{
		return 'https://'.CCIn::host().static::to( $uri, $params, $retain );
	}

	/**
	 * Get the current url
	 *
	 * @param array  	$params
	 * @param bool		$retain		Should we keep the get parameters?
	 * @return string 
	 */
	public static function current( $params = array(), $retain = false ) 
	{
		return static::to( CCIn::uri(), $params, $retain );
	}

	/**
	 * Get the url to a action of the current route
	 *
	 * !Important it's not possible to link between action on multiple routes
	 * This method always assumes that all actions are in the same route.
	 * If you want to link to another route using an alias you could do something like:
	 *
	 * CCUrl::to( '@myalias/myaction' );
	 *
	 * @param string 	$action
	 * @param array  	$params
	 * @param bool		$retain		Should we keep the get parameters?
	 * @return string 
	 */
	public static function action( $action = '', $params = array(), $retain = false ) 
	{
		if ( $action == 'index' )
		{
			$action = '';
		}

		if ( CCRequest::current() && ( $route = CCRequest::current()->route ) )
		{
			$uri = $route->uri;
			
			if ( !is_null( $route->action ) )
			{
				$uri =  substr( $uri, 0, strlen( $route->action ) * -1 );
			}

			if ( substr( $uri, -1 ) != '/' )
			{
				$uri .= '/';
			}

			return static::to( $uri.$action, $params, $retain );
		}

		throw new CCException( 'CCUrl::action - There has been no route executed yet.' );	
	}

	/**
	 * Is the given url active?
	 * This function ignores the domain and the parameters if the
	 * uri matches the current uri true will be returned.
	 *
	 * @param string 		$url	
	 * @return bool
	 */
	public static function active( $url )
	{
		$url = parse_url( $url, PHP_URL_PATH );

		if ( empty( $url ) )
		{
			return false;
		}

		if ( $url[0] !== '/' )
		{
			$url = static::to( $url );
		}

		// when we are on "/" only "/" counts as active.
	 	if ( $url === '/' )
		{
			return static::current() == $url;
		}

		// for everything else we cut the url and compare
		$cut = substr( static::current(), 0, strlen( $url ) );

		return $cut == $url;
	}
}