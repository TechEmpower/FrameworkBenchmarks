<?php 
/*
 *---------------------------------------------------------------
 * Application Object
 *---------------------------------------------------------------
 *
 * This is your default application object.
 */
class App extends CCApp 
{	
	/**
	 * The application name
	 *
	 * @var string
	 */
	public static $name = 'My Application';
	
	/**
	 * App configuration
	 *
	 * @var CCConfig
	 */
	public static $config = null;
	
	/**
	 * current user object
	 *
	 * @var User
	 */
	public static $user = null;
	
	/**
	 * Application initialization.
	 * do your inital stuff here like getting the current user object ect..
	 * You can return a CCResponse wich will cancle all other actions 
	 * if enebaled ( see. main.config -> send_app_wake_response )
	 *
	 * @return void | CCResponse
	 */
	public static function wake() 
	{
		/*
		 * Start the session by adding the current uri
		 */
		//CCSession::set( 'uri', CCServer::uri() );
		
		/*
		 * try to authenticate the user
		 */
		//static::$user =& CCAuth::handler()->user;
		
		/*
		 * load the App configuration
		 */
		//static::$config = CCConfig::create( 'app' );
	}
	
	/**
	 * Environment wake
	 * This is an environment wake they get called if your running
	 * the application in a special environment like:
	 *     wake_production
	 *     wake_phpunit
	 *     wake_test
	 *
	 * @return void | CCResponse
	 */
	public static function wake_development() 
	{	
		CCProfiler::check( 'App::development - Application finished wake events.' );
	}
}