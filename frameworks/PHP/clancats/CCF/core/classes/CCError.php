<?php namespace Core;
/**
 * Errors
 * Catch error and exceptions and process them with a
 * custom error handler and inspector.
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */

/**
 * The error class
 */
class CCError {

	/**
	 * non fatal errors
	 * 
	 * @var array
	 */
	public static $non_fatals = array();

	/**
	 * is ccf in the shutdown proceess?
	 *
	 * @var bool
	 */
	private static $in_shutdown_process = false;
	
	/**
	 * error types
	 *
	 * @var array
	 */
	protected static $error_types = array(
		E_ERROR				=> 'error',
		E_WARNING         	=> 'warning',
		E_PARSE           	=> 'parsing error',
		E_NOTICE          	=> 'notice',
		E_CORE_ERROR      	=> 'core error',
		E_CORE_WARNING    	=> 'core warning',
		E_COMPILE_ERROR   	=> 'compile error',
		E_COMPILE_WARNING 	=> 'compile warning',
		E_USER_ERROR      	=> 'user error',
		E_USER_WARNING    	=> 'user warning',
		E_USER_NOTICE     	=> 'user notice',
		E_STRICT 		  	=> 'runtime notice',
		E_RECOVERABLE_ERROR 	=> 'recoverable error',
		E_DEPRECATED			=> 'deprecated error',
		
	);

	/**
	 * error class init
	 *
	 * @return void
	 */
	public static function _init() 
	{
		// we capture non fatal errors only in dev environments
		if ( ClanCats::in_development() ) 
		{
			// add a hook to the main resposne
			CCEvent::mind( 'response.output', function( $output ) {
				
				if ( strpos( $output, '</body>' ) === false ) 
				{
					return $output;
				}
				
				$table = \UI\Table::create( array(
					 'style'		=> array(
					 	'width'	=> '100%',
					 ),
					 'cellpadding' 	=> '5',
					 'class'			=> 'table debug-table debug-table-errors',
				));
				
				$table->header( array( '#', 'message', 'file' ) );
				
				foreach( \CCError::$non_fatals as $key => $item )
				{
					$table->row( array( 
						$key+1,
						$item->getMessage(),
						CCStr::strip( $item->getFile(), CCROOT ).':'.$item->getLine(), 
					));
				}
			
				return str_replace( '</body>', $table."\n</body>", $output );
			});
		}
	}
	
	/**
	 * check if a level is fatal
	 *
	 * @param int		$level
	 * @return bool
	 */
	private static function is_fatal_level( $level )
	{
		$fatals = array(
			E_ERROR,
			E_PARSE,
			E_CORE_ERROR,
			E_CORE_WARNING,
			E_COMPILE_ERROR,
			E_COMPILE_WARNING,
			E_DEPRECATED,
		);
		
		if ( is_object( ClanCats::$config ) && ClanCats::$config instanceof CCConfig ) 
		{
			if ( in_array( $level, ClanCats::$config->get( 'error.fatal_levels', $fatals ) )) 
			{
				return true;
			}
		}
		
		return false;
	}
	
	/**
	 * exception handler
	 *
	 * @param Exception 		$exception
	 * @return void
	 */
	public static function exception( $exception ) 
	{
		// clean the main output buffer
		while ( ob_get_level() > 0 ) 
		{
			ob_end_clean();
		}
		
		$error_handler = "\\".CCCORE_NAMESPACE."\\CCError_Handler".( ClanCats::is_cli() ? '_Cli' : '' );
		$error_inspector = "\\".CCCORE_NAMESPACE."\\CCError_Inspector";
		
		if ( is_object( ClanCats::$config ) && ClanCats::$config instanceof CCConfig ) 
		{
			$error_handler = ClanCats::$config->get( 'error.'.( ClanCats::is_cli() ? 'cli_' : '' ).'handler', $error_handler );
			$error_inspector = ClanCats::$config->get( 'error.inspector', $error_inspector );
		}
		
		$inspector = new $error_inspector( $exception );
		
		$handler = new $error_handler( $inspector );
		
		$handler->handle();
	}

	/**
	 * The error handler converts PHP errors to CCExceptions.
	 * These get handled by the CCError exception handler.
	 *
	 * Also the error handler decides the fatality of an error.
	 *
	 * @param CCException 	$exception
	 * @return void
	 */
	public static function error( $num, $message, $file, $line ) 
	{	
		$exception = new CCException( static::$error_types[$num]." - ".$message, 0, $num, $file, $line );
		
		if ( static::is_fatal_level( $num ) ) 
		{
			if ( static::$in_shutdown_process ) 
			{
				static::exception( $exception );
			} else 
			{
				throw $exception;
			}
		} else 
		{
			static::$non_fatals[] = $exception; 
		}
	}

	/**
 	 * shutdown handler
 	 *
 	 * @return void
 	 */
	public static function shutdown() 
	{	
		// enter shutdown context
		static::$in_shutdown_process = true;
		
		$error = error_get_last();
		
		// if the error is not null and the level is 
		// fatal. Forward the error to our error handler.
		if ( !is_null( $error ) && static::is_fatal_level( $error['type'] ) ) 
		{
			static::error( $error['type'], $error['message'], $error['file'], $error['line'] );
		}
	}
}