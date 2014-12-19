<?php namespace Core;
/**
 * Command line interface
 * Some helpers when using the command line interface.
 *
 * FuelPHP helped me alot with this class thanks your awesome framework guys.
 * Go check it out: http://fuelphp.com
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCCli 
{
	
	/**
	 * foreground colors
	 * 
	 * @var array
	 */
	protected static $colors = array(
		'black'			=> '0;30',
		'dark_gray'		=> '1;30',
		'blue'			=> '0;34',
		'dark_blue'		=> '1;34',
		'light_blue'		=> '1;34',
		'green'			=> '0;32',
		'success'		=> '0;32',
		'light_green'	=> '1;32',
		'cyan'			=> '0;36',
		'info'			=> '0;36',
		'light_cyan'		=> '1;36',
		'red'			=> '0;31',
		'error'			=> '0;31',
		'light_red'		=> '1;31',
		'purple'			=> '0;35',
		'light_purple'	=> '1;35',
		'light_yellow'	=> '0;33',
		'yellow'			=> '1;33',
		'warning'		=> '1;33',
		'light_gray'		=> '0;37',
		'white'			=> '1;37',
	);
	
	/**
	 * background colors
	 * 
	 * @var array
	 */
	protected static $background_colors = array(
		'black'			=> '40',
		'red'			=> '41',
		'green'			=> '42',
		'yellow'			=> '43',
		'blue'			=> '44',
		'magenta'		=> '45',
		'cyan'			=> '46',
		'light_gray'		=> '47',
	);
	
	/**
	 * Check if php is running on windows cmd
	 * 
	 * @return bool
	 */
 	public static function is_windows() 
 	{ 
 		return strtolower( substr( PHP_OS, 0, 3 ) ) === 'win';
 	}
	
	/**
	 * Write to the standard output device
	 * 
	 * @param string		$str
	 * @return void
	 */
	public static function write( $str, $color = null, $background = null ) 
	{
		if ( !is_null( $color ) ) 
		{
			$str = static::color( $str, $color, $background );
		}
		
		fwrite( STDOUT, $str );
	}
	
	/**
	 * Write to the standard output device with an end of line.
	 * 
	 * @param string		$str
	 * @return void
	 */
	public static function line( $str, $color = null, $background = null ) 
	{
		if ( is_array( $str ) ) 
		{
			$str = implode( PHP_EOL, $str );
		}
		
		static::write( $str.PHP_EOL, $color, $background );
	}
	
	/**
	 * Display a success message
	 *
	 * @param string		$str
	 * @param string		$prefix
	 * @return void
	 */ 
	public static function success( $str, $prefix = 'success' )
	{
		static::line( static::color( $prefix, 'green' ).' '.$str );
	}
	
	/**
	 * Display an error message
	 *
	 * @param string		$str
	 * @param string		$prefix
	 * @return void
	 */ 
	public static function error( $str, $prefix = 'failure' )
	{
		static::line( static::color( $prefix, 'red' ).' '.$str );
	}
	
	/**
	 * Display a warning message
	 *
	 * @param string		$str
	 * @param string		$prefix
	 * @return void
	 */ 
	public static function warning( $str, $prefix = 'warning' )
	{
		static::line( static::color( $prefix, 'yellow' ).' '.$str );
	}
	
	/**
	 * Display an info message
	 *
	 * @param string		$str
	 * @param string		$prefix
	 * @return void
	 */ 
	public static function info( $str, $prefix = 'info' )
	{
		static::line( static::color( $prefix, 'cyan' ).' '.$str );
	}
	
	/**
	 * Write empty lines 
	 * 
	 * @param int		$count
	 * @return void
	 */
	public static function new_line( $count = 1 ) 
	{
		for( $i=0; $i<$count; $i++ ) 
		{
			static::line( '' );
		}
	}
	
	/**
	 * Add color to string
	 * Check the available colors at the top $colors & $background_colors
	 * 
	 * @param string 	$str
	 * @param string		$color
	 * @param string		$background
	 * @return string
	 */
	public static function color( $str, $color, $background = null ) 
	{	
		// ANSI escape codes are not available on Windows CMD
		// So we return the string unmodified
		if ( static::is_windows() ) 
		{
			return $str;
		}
		
		$out = "\033[".static::$colors[$color]."m";
		
		if ( !is_null( $background ) ) 
		{
			$out .= "\033[".static::$background_colors[$background]."m";
		}
		
		return $out.$str."\033[0m";
	}
	
	/**
	 * Clear the screen
	 *
	 * @return void
	 */
	public static function clear() 
	{
		if ( static::is_windows() ) 
		{
			static::new_line(40); return;
		}
		
		static::write( chr(27)."[H".chr(27)."[2J" );
	}
	
	/**
	 * Get a bool value from the user
	 * This will write the question down and waiting for a yes or no.
	 *
	 * When strict is true, it wil only accept yes or no. And not y, n, jep etc.
	 *
	 * @param string		$question
	 * @param bool		$strict
	 * @return bool
	 */
	public static function confirm( $question, $strict = false ) 
	{
		$question .= ' [yes/no]: ';
		
		if ( $strict ) 
		{
			do 
			{
				$res = strtolower( static::read( $question ) );
			} 
			while ( $res !== 'yes' && $res !== 'no' );
			
			return ( $res == 'yes' ) ? true : false;
		}
		
		do 
		{
			$res = strtolower( static::read( $question ) );
		} 
		while ( empty( $res ) );
		
		$positives = array(
			'yes', 'y', 'ya', 'ye', 'yeah', 'yup',
			'jep', 'jap', 'ja', 
			'si', 'sim',
			'true',
			'hai',
			'oui',
			'no problemo',
		);
		
		return in_array( $res, $positives );
	}

	/**
	 * Read from comman line 
	 * Because windows does not support readline we use normal fgets in that case.
	 * 
	 * @param string	 		$prefix		The prefix will be printet before the user can respond.
	 * @return string
	 */
	public static function read( $prefix = '' ) 
	{
		if ( !static::is_windows() ) 
		{	
			$line = readline( $prefix ); 
			readline_add_history( $line );
		} 
		else 
		{	
			static::write( $prefix );
			$line = trim( fgets( STDIN ) );
		}
		
		return $line;
	}
}
