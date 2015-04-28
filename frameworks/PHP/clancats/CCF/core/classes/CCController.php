<?php namespace Core;
/**
 * Controller 
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCController 
{	
	/**
	 * default action
	 *
	 * @var string
	 */
	protected static $_default_action = 'index';
	
	/**
	 * action method prefix
	 *
	 * @var string
	 */
	protected static $_action_prefix = 'action_';
	
	/**
	 * action handler 
	 *
	 * @var string
	 */
	protected static $_action_handler = null;
	
	/**
	 * CCController factory
	 *
	 * @param string			$path
	 * @return CCController
	 */
	public static function create( $path ) 
	{
		$class = static::find( $path );
		
		// if class already loaded
		if ( !class_exists( $class, false ) ) 
		{
			// register the controller with the autoloader
			\CCFinder::bind( $class, CCPath::get( $path, CCDIR_CONTROLLER, 'Controller'.EXT ) );
		}
		
		// create new controller instance and assign the name
		$controller = new $class;
		$controller->name = $path;
		
		return $controller;
	}
	
	/**
	 * get the class by a path and register it to the autoloader 
	 *
	 * @param string		$path
	 * @return string
	 */
	public static function find( $path ) 
	{
		// the there is always a Controller suffix
		$path .= 'Controller';
		
		// get the class
		$class = str_replace( array( '/', '::' ), array( '_', '\\' ), $path );
		
		// return the class name
		return $class;
	}
	
	/**
	 * check if a controller implements an action
	 *
	 * @param string		$path
	 * @param string		$action
	 * @return bool
	 */
	public static function has_action( $path, $action = null ) 
	{
		$path = CCStr::cut( $path, '@' );
		$path = CCStr::cut( $path, '?' );
		
		// fix closure given
		if ( !is_string( $path ) )
		{
			return false;	
		}
		
		// get controllers default action
		if ( is_null( $action ) ) 
		{
			$action = static::$_default_action;
		}
		
		// try to load the controller
		if ( !$controller = static::create( $path ) ) 
		{
			return false;
		}
		
		return method_exists( $controller, static::$_action_prefix.$action );
	}
	
	/**
	 * the response
	 *
	 * @var CCResponse
	 */
	protected $response = null;
	
	/**
	 * the controller output
	 *
	 * @var string
	 */
	protected $output = null;
	
	/**
	 * defualt status
	 * 
	 * @var int
	 */
	protected $status = 200;
	
	/**
	 * The current controller name
	 *
	 * @param string
 	 */
	protected $name = null;
	
	/**
	 * Create the response object from an string
	 * This function is needed for some subclasses
	 * just like the view controller
	 * 
	 * @param string
	 * @return CCResponse
	 */
	protected function _respond( $string )
	{
		return CCResponse::create( $string, $this->status );
	}
	
	/**
	 * Creates the language prefixes wich allows us 
	 * to use the short :action and :controller translations
	 *
	 * @param string 		$action
	 * @return void
 	 */
	protected function set_language_alias( $action )
	{
		// create langugage aliases
		$name = explode( '::', $this->name );
		
		if ( isset( $name[1] ) )
		{
			$prefix = $name[0].'::';
			$name = $name[1];
		}
		else
		{
			$prefix = '';
			$name = $name[0];
		}
		
		if ( empty( $action ) )
		{
			$action = static::$_default_action;
		}
		
		CCLang::alias( ':controller', $prefix.'controller/'.strtolower( $name ) );
		CCLang::alias( ':action', $prefix.'controller/'.strtolower( $name.'.'.$action ) );
	}
	
	/**
	 * controller execution
	 * 
	 * @param string		$action
	 * @param array 		$params
	 * @return CCResponse
	 */
	public function execute( $action = null, $params = array() ) 
	{
		$this->set_language_alias( $action );
		
		// reset the response
		$this->response = null;
		$this->output = null;
		
		// fix notice
		$return = null;
		 
		if ( is_null( $action ) ) 
		{
			$action = static::$_default_action;
		}
		
		// capture all output made in the controller
		ob_start();
		
		// run wake event
		if ( method_exists( $this, 'wake' ) ) 
		{
			$return = call_user_func_array( array( $this, 'wake' ), $params );
		}
		
		// do we already have an resposne from the wake event?
		if ( !$return instanceof CCResponse )
		{
			// check if we got a custom action handler
			if ( !is_null( static::$_action_handler ) ) 
			{
				$return = call_user_func( array( $this, static::$_action_handler ), $action, $params );
			} 
			// normal action
			else
			{
				// check if the action exists
				if ( method_exists( $this, static::$_action_prefix.$action ) )
				{
					$return = call_user_func_array( array( $this, static::$_action_prefix.$action ), $params );
				}
				else 
				{
					$return = CCResponse::error( 404 );
				}
			}
		}
		
		// get the output
		$this->output = ob_get_clean();
		
		// do we got a response now?
		if ( $return instanceof CCResponse ) 
		{
			$this->response = $return;
		}
		// or a string
		elseif ( !empty( $return ) && is_string( $return ) )
		{
			$this->response = $this->_respond( $return );
		}
		
		// do we got a valid response if not use controller output		
		if ( !$this->response instanceof CCResponse ) 
		{
			$this->response = $this->_respond( $this->output );
		}
		
		// run sleep event
		if ( method_exists( $this, 'sleep' ) ) 
		{
			call_user_func_array( array( $this, 'sleep' ), $params );
		}
		
		// return thaaaaat
		return $this->response;
	}
}