<?php namespace Core;
/**
 * Profiler / Debugger 
 * Profiling and debuggin helpers
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCProfiler 
{
	/**
	 * Is the profile enabled
	 *
	 * @var bool 
	 */
	private static $_enabled = false;
	
	/**
	 * Profiler data store
	 *
	 * @var array
	 */
	protected static $_data = array();
	
	/**
	 * The Autoloader initialisation
	 *
	 * @return void
	 */
	public static function _init() 
	{
		// is the profiler enabled?
		if ( !static::$_enabled = ClanCats::$config->get( 'profiler.enabled' ) )
		{
			return;
		}
		
		// enable profiling only in development mode
		if ( ClanCats::in_development() ) 
		{					
			// add a hook to the resposne so that we can 
			// append a table with the profiler data to the body
			CCEvent::mind( 'response.output', function( $output ) 
			{	
				if ( strpos( $output, '</body>' ) === false ) 
				{
					return $output;
				}
				
				$table = \UI\Table::create( array(
					 'style'		=> array(
					 	'width'	=> '100%',
					 ),
					 'cellpadding' 	=> '5',
					 'class'			=> 'table debug-table debug-table-profiler',
				));
				
				$table->header( array( '#', 'message', 'memory', 'time' ) );
				
				foreach( \CCProfiler::data() as $key => $item )
				{
					$table->row( array( $key+1, $item[0], $item[1], $item[2] ) );
				}
				
				// add the table before the body end
				return str_replace( '</body>', $table."\n</body>", $output );
			});
			
			// also add an error inspector hook so that we can access the 
			// profiler data in the error handler
			CCError_Inspector::info_callback( 'Profiler', function() 
			{	
				$table = array();
				
				foreach( \CCProfiler::data() as $key => $check )
				{
					$table[('#'.($key+1).': '.$check[2] )] = $check[0];
				}
				
				return $table;
			});
		}
	}
	
	/**
	 * Enables the profiler
	 *
	 * @return array
	 */
	public static function enable()
	{
		return static::$_enabled = true;
	}
	
	/**
	 * Disables the profiler
	 *
	 * @return array
	 */
	public static function disable()
	{
		return static::$_enabled = false;
	}
	
	/**
	 * Returns the current profiler data
	 *
	 * @return array
	 */
	public static function data()
	{
		return static::$_data;
	}
	
	/**
	 * Resets all profiler data
	 *
	 * @return void
	 */
	public static function reset()
	{
		static::$_data = array();
	}
	
	/**
	 * returns current memory usage
	 *
	 * @param bool 		$format
	 * @return string
	 */
	public static function memory( $format = false )
	{	
		$memory = memory_get_usage() - CCF_PROFILER_MEM;
		
		if ( $format )
		{
			return CCStr::bytes( $memory );
		}
		
		return $memory;
	}
	
	/**
	 * returns current execution time
	 *
	 * @param bool 		$format
	 * @return string
	 */
	public static function time( $format = false )
	{	
		$time = microtime( true ) - CCF_PROFILER_TME;
		
		if ( $format )
		{
			return CCStr::microtime( $time );
		}
		
		return $time;
	}
	
	/**
	 * Make a profiler checkpoint
	 *
	 * @param string 	$message		just a short message what here happend
	 * @return void
	 */
	public static function check( $message ) 
	{
		if ( !static::$_enabled )
		{
			return;
		}
		
		static::$_data[] = array( $message, static::memory( true ), static::time( true ) );
	}
}