<?php namespace Core;
/**
 * ClanCatsFramework main class
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class ClanCats 
{
	/**
	 * The current ccf version
	 *
	 * @var string
	 */
	const VERSION 	= '2.0.0';
	
	/**
	 * The current CCF environment
	 *
	 * @var string
	 */
	private static $environment = null;
	
	/**
	 * The runtime application class name
	 *
	 * @var string
	 */
	private static $runtime_class = null;
	
	/**
	 * Is ccf running on a cli?
	 *
	 * @var bool
	 */
	private static $is_cli = null;
	
	/**
	 * Is ccf in in development mode?
	 *
	 * @var bool
	 */
	private static $in_development = null;
	
	/**
	 * The defined paths 
	 *
	 * @var array
	 */
	private static $paths = array();
	
	/**
	 * The defined directories 
	 *
	 * @var array
	 */
	private static $directories = array();
		
	/**
	 * The base configuration
	 *
	 * @var CCConfig
	 */
	public static $config = null;
	
	/***
	 * get the curren ccf version
	 *
	 * @return string
	 */
	public static function version()
	{
		return static::VERSION;
	}
	
	/**
	 * get the current environment
	 * example:
	 *     development
	 *     production
	 *     staging
	 *
	 * @return string
	 */
	public static function environment()
	{
		return static::$environment;
	}
	
	/**
	 * check if the current env equals the passed string
	 *
	 * @param string			$env
	 * @return bool
	 */
	public static function environment_is( $env )
	{
		return $env == static::$environment;
	}
	
	/**
	 * get the current runtime class name
	 * or execute an function on the runtime class
	 * 
	 * @return string
	 */
	public static function runtime( $fnc = null, $params = array() )
	{
		if ( is_null( $fnc ) )
		{
			return static::$runtime_class;
		}
		
		return call_user_func_array( array( static::$runtime_class, $fnc ), $params );
	}
	
	/**
	 * check if we are running ccf from the console
	 *
	 * @return bool
	 */
	public static function is_cli()
	{
		if ( is_null( static::$is_cli ) )
		{
			return static::$is_cli = ( php_sapi_name() == 'cli' ) ? true : false; 
		}
		return static::$is_cli;
	}
	
	/**
	 * check if the current environment is development
	 *
	 * @return bool
	 */
	public static function in_development()
	{
		if ( is_null( static::$in_development ) )
		{
			return static::$in_development = ( static::environment_is( 'development' ) || static::environment_is( 'phpunit' ) );
		}
		return static::$in_development;
	}
	
	/**
	 * paths getter and setter
	 * 
	 * when paths empty: 
	 *     return all registerd paths
	 *
	 * when paths an array:
	 *     adds paths to the index and optional create a define.
	 *
	 * @param array 			$paths
	 * @param bool			$define
	 * @return array|void
	 */
	public static function paths( $paths = null, $define = true )
	{
		if ( is_null( $paths ) )
		{
			return static::$paths;
		}
		
		foreach( $paths as $key => $path )
		{
			static::$paths[$key] = $path;
			
			if ( $define === true )
			{
				define( strtoupper( $key ).'PATH', $path );
			}
		}
	}
	
	/**
	 * directories getter and setter
	 * 
	 * when dirs empty: 
	 *     return all registerd directories
	 *
	 * when dirs an array:
	 *     adds directories to the index and optional create a define.
	 *
	 * @param array 			$paths
	 * @param bool			$define
	 * @return array|void
	 */
	public static function directories( $dirs = null, $define = true )
	{
		if ( is_null( $dirs ) )
		{
			return static::$directories;
		}
		
		foreach( $dirs as $key => $dir )
		{
			static::$directories[$key] = $dir;
			
			if ( $define === true )
			{
				define( 'CCDIR_'.strtoupper( $key ), $dir );
			}
		}
	}
	
	/** 
	 * Returns the direcotry name
	 *
	 * @param string 		$dir
	 * @return string|false
	 */
	public static function directory( $dir )
	{
		if ( array_key_exists( $dir, static::$directories ) )
		{
			return static::$directories[$dir];
		}
		
		return false;
	}
	
	/**
	 * Detect the environment by the given parameter.
	 * You can pass arrays, strings and callbacks:
	 * 
	 * array:
	 *     // if you pass an array the detector is going
	 *     // to try to match the HTTP_HOST as key.
	 *     array( 'localhost' => 'development', ':all' => 'production' )
	 *
	 * string: 
	 *     return 'staging'
	 *
	 * callback:
	 *     function() { return $_SERVER['CCF_ENV']; }
	 *
	 * If the detector fails he wil return the default environment.
	 *
	 * @param mixed 			$env
	 * @return string
	 */
	public static function environment_detector( $env )
	{
		$detected_environment = 'development';
		
		if ( isset( $env ) && !empty( $env ) )
		{
			if ( is_string( $env ) )
			{
				$detected_environment = $env;
			}
			elseif ( is_array( $env ) )
			{
				if ( isset( $env[':all'] ) )
				{
					$detected_environment = $env[':all'];
				}
				
				foreach( $env as $host => $host_env )
				{
					$rgx = "~^".str_replace( '*', '(.*)', $host )."$~";
					
					if ( preg_match( $rgx, $_SERVER['HTTP_HOST'] ) )
					{
						$detected_environment = $host_env;
					}
				}
			}
			elseif ( is_callable( $env ) )
			{
				return call_user_func( $env );
			}
		}
				
		return $detected_environment;
	}
	
	/**
	 * start the ccf lifecycle
	 *
	 * this method sets the current environment, loads the configuration
	 * and wakes the application
	 *
	 * @param string			$environment
	 * @return void
	 */
	public static function wake( $environment ) 
	{
		if ( !is_null( static::$environment ) )
		{
			throw new CCException( "ClanCats::wake - you cannot wake the application twice." );
		}	
		
		// set environment
		static::$environment = $environment;
		
		// load the main configuration
		static::$config = CCConfig::create( 'main' );
		
		// setup the application error tables
		CCError_Inspector::info_callback( 'ClanCatsFramework', function() { 
			return array(
				'Runtime Class'		=> \ClanCats::runtime(),
				'CCF Version'		=> \ClanCats::version(),
				'CCF Environment'	=> \ClanCats::environment(),
				'Development env'	=> var_export( \ClanCats::in_development(), true ),
				'File extention'		=> EXT,
				'Core namespace'		=> CCCORE_NAMESPACE,
			);
		});
		
		CCError_Inspector::info_callback( 'CCF Paths', array( 'ClanCats', 'paths' ) );
		CCError_Inspector::info_callback( 'CCF Directories', array( 'ClanCats', 'directories' ) );
		CCError_Inspector::info_callback( 'Namespaces', function() {
			return \CCFinder::$namespaces;
		});
	}
	
	/**
	 * start the ccf app lifecycle
	 *
	 * This method registers the App class and runs the wake events.
	 *
	 * @param string			$app		The used app class = APPATH/<$app>.php
	 * @return void
	 */
	public static function wake_app( $app )
	{
		static::$runtime_class = $app;
		\CCFinder::bind( $app, static::$paths['app'].$app.EXT );
		
		// run the application wake
		$response = $app::wake();
		
		// when the application wake returns an response we display it
		if ( $response instanceof CCResponse ) {
			if ( static::$config->send_app_wake_response ) {
				$response->send( true ); die;	
			}
		}
		
		$response = null;
		
		// run the environment wake
		if ( method_exists( $app, 'wake_'.static::$environment ) )
		{
			$response = call_user_func( $app.'::wake_'.static::$environment );
			
			// when the application env wake returns an response we display it
			if ( $response instanceof CCResponse ) {
				if ( static::$config->send_app_wake_response ) {
					$response->send( true ); die;	
				}
			}
		}
		
		// add routes from the app
		CCRouter::on( $app::routes() );
	}
}