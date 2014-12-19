<?php namespace Core;
/**
 * Base error inspector
 * This inspector wil try to get more information out 
 * of an exception.
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 * 
 */
class CCError_Inspector
{
	/**
	 * An array of callback that return info arrays
	 *
	 * @var array
	 */
	protected static $info_callbacks = array();
	
	/**
	 * Add an info callback to the error inspector.
	 * The inspector will execute these callback and use the returned
	 * data for helping.
	 *
	 * @param string		$name
	 * @param mixed		$callback
	 * @return void
	 */
	public static function info_callback( $name, $callback )
	{
		if ( !is_callable( $callback ) )
		{
			throw new \Exception( "CCError_Inspector::info_callback - invalid callback given." );
		}
		
		static::$info_callbacks[$name] = $callback;
	}	
	
	/**
	 * The current error inspector
	 *
	 * @var \Exception
	 */
	protected $exception = null;
	
	/**
	 * The inspector trace
	 *
	 * @var array
	 */
	protected $trace = null;
	
	/**
	 * The inspector tables
	 *
	 * @var array
	 */
	protected $tables = null;
	
	/**
	 * instance contrucor
	 *
	 * @param \Exception		$exception
	 * @return void
	 */
	public function __construct( \Exception $exception )
	{
		$this->exception = $exception;
	}
	
	/**
	 * get the current exception
	 *
	 * @return Exception
	 */
	public function exception()
	{
		return $this->exception;
	}
	
	/**
	 * get the current exception
	 *
	 * @return Exception
	 */
	public function exception_name()
	{
		return get_class( $this->exception );
	}
	
	/**
	 * get the current exception
	 * also replace full path information to more relative style
	 *
	 * @return Exception
	 */
	public function message()
	{
		return str_replace( CCROOT, '/', $this->exception->getMessage() );
	}
	
	/**
	 * get the inspector trace
	 *
	 * @param bool		$add_self	Adds the current exception also to trace.
	 */
	public function trace( $add_self = false )
	{
		if ( is_null( $this->trace ) )
		{
			$this->trace = array();
			
			if ( $add_self )
			{
				$this->trace[] = new CCError_Trace( array(
					'file'	=> $this->exception->getFile(),
					'line'	=> $this->exception->getLine(),
					'class'	=> $this->exception_name(),
				));
			}
			
			foreach( $this->exception->getTrace() as $trace )
			{
				$this->trace[] = new CCError_Trace( $trace );
			}
		}
		
		return $this->trace;
	}
	
	/**
	 * returns all debug tables
	 *
	 * @return array
	 */
	public function tables()
	{
		$tables = array();
		
		foreach( static::$info_callbacks as $name => $callback )
		{
			$tables[$name] = call_user_func( $callback );
		}
		
		return array_merge( $tables , array(
			'Server / request data' => $_SERVER,
			'Get parameters' => $_GET,
			'Post parameters' => $_POST,
			'Cookies' => $_COOKIE,
			'Files' => $_FILES,
		));
	}
}