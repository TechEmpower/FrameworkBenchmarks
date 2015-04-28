<?php namespace Core;
/**
 * An Request Object handler
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCRequest 
{
	/**
	 * stores the current request object
	 * 
	 * @var CCRequest
	 */
	private static $_current = null;
	
	/**
	 * returns teh current request object
	 *
	 * @return CCResquest
	 */
	public static function current()
	{
		return static::$_current;
	}
		
	/**
	 * create an request using an input instance
	 * 
	 * @param CCIn_Instance 		$input
	 * @return CCRequest
	 */
	public static function with( CCIn_Instance $input ) 
	{
		return new static( CCRouter::resolve( $input->uri() ), $input );
	}
	
	/**
	 * creates an request object using the passed uri
	 *
	 * @param string 			$uri
	 * @param CCIn_Instance 		$input
	 * @return CCRequest
	 */
	public static function uri( $uri, $input = null ) 
	{		
		return new static( CCRouter::resolve( $uri ), $input );
	}
	
	/**
	 * default request factory
	 *
	 * @param CCRoute			$route
	 * @param CCIn_Instance 		$input
	 * @return CCRequest
	 */
	public static function create( CCRoute $route, $input = null ) 
	{
		return new static( $route, $input );
	}
	
	/*
	 * input holder
	 */
	public $input = null;
		
	/*
	 * route holder
	 */
	public $route = null;
	
	/*
	 * response holder
	 */
	public $response = null;
	
	/*
	 * Request Arguments
	 */
	public $args;
	
	/**
	 * CCRequest constructor
	 * 
	 * @param CCRoute			$route
	 * @param CCIn_Instance 		$input
	 */
	public function __construct( $route, $input = null ) 
	{	
		// assign the route and input
		$this->route = $route;
		$this->input = $input;
		
		// create an object with default arguments
		$this->args = CCArr::object( ClanCats::$config->get( 'controller.default_args') );
	}
	
	/**
	 * Execute the Request
	 *
	 * @param array 	$action
	 * @param array 	$params
	 *
	 * @return self
	 */
	public function perform() 
	{	
		// set the input
		if ( !is_null( $this->input ) ) 
		{
			CCIn::instance( $this->input );
		} else {
			CCIn::instance( CCServer::instance() );
		}
		
		// set current request
		static::$_current =& $this;
		
		// route is invalid show 404
		if ( !$this->route instanceof CCRoute ) 
		{
			$this->route = CCRouter::resolve( '#404' );
		}
		
		/*
		 * call wake events
		 * if one event returns an response all other calls will be skipped also events!
		 */
		foreach( CCRouter::events_matching( 'wake', $this->route->uri ) as $callback )
		{
			if ( ( $return = CCContainer::call( $callback ) ) instanceof CCResponse )
			{
				$this->response = $return; return $this;
			}
		}
		
		/*
		 * a closure
		 */
		if ( !is_array( $this->route->callback ) && is_callable( $this->route->callback ) ) 
		{	
			// execute and capture the output
			ob_start();
			// run the closure 
			$return = call_user_func_array( $this->route->callback, $this->route->params );
			// catch the output
			$output = ob_get_clean();
			
			// do we got a response?
			if ( !$return instanceof CCResponse ) 
			{
				// if not create one with the captured output
				$return = CCResponse::create( $output );
			}
		}
		/*
		 * a callback
		 */
		elseif ( is_callable( $this->route->callback ) )
		{
			// execute the callback and get the return
			$return = call_user_func_array( $this->route->callback, array( $this->route->action, $this->route->params ) );
			
			// do we got a response?
			if ( !$return instanceof CCResponse ) 
			{
				// if not create one with the return as string
				$return = CCResponse::create( (string) $return );
			}
		}
		/*
		 * 404 error if nothing
		 */
		else 
		{
			$return = CCResponse::error( 404 );
		}
		
		// set the response
		$this->response = $return;
		
		/*
		 * call sleep events
		 * if one event returns an response all other calls will be skipped also events!
		 */
		foreach( CCRouter::events_matching( 'sleep', $this->route->uri ) as $callback )
		{
			if ( $return = CCContainer::call( $callback, $this->response ) instanceof CCResponse )
			{
				$this->response = $return; return $this;
			}
		}
		
		return $this;
	}
	
	/**
	 * get the response of the current request
	 *
	 * @return CCResponse
	 */
	public function response() 
	{
		return $this->response;
	}
}