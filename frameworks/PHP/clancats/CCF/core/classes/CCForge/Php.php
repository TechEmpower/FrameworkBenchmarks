<?php namespace Core;
/**
 * PHP Code Forge
 * generates php files
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCForge_Php {
	
	/**
	 * creates an new CCForge instance
	 *
	 * @param string 		$namespace
	 * @return CCForge_Php
	 */
	public static function create( $namespace = null )
	{
		return new static( $namespace );
	}
	
	/**
	 * runs a forge command on a dummy forge
	 *
	 * @param string		$cmd
	 * @param array 		$params
	 * @return string
	 */
	public static function make( $cmd, $params )
	{
		if ( !is_array( $params ) )
		{
			$params = array( $params );
		}
		
		$forge = new static;
		
		if ( !method_exists( $forge, $cmd ) )
		{
			throw new CCException( "CCForge_Php - Command could not be found." );
		}
		
		return call_user_func_array( array( $forge, $cmd ), $params );
	}
	
	/*
	 * the buffer of the current file
	 */
	public $buffer = "";
	
	/**
	 * get the current buffer
	 *
	 * @return string
	 */
	public function __toString() 
	{
		return $this->buffer;
	}
	
	/**
	* generates an PHP file header
	*
	* @param string		$namespace
	* @return string
	*/
	public function __construct( $namespace = null ) 
	{
		$this->header( $namespace );
	}
	
	/**
	 * add a string to the buffer
	 *
	 * @param string		$str
	 * @return string 	the given string without breaks
	 */
	public function add( $str )
	{
		$this->buffer .= $str."\n"; return trim( $str ); 
	}
	
	/**
	 * creates empty lines
	 *
	 * @param int	$count
	 * @return void
	 */
	public function line( $count = 1 )
	{
		$return = "";
		
		for( $i=0;$i<$count;$i++ )
		{
			$this->buffer .= "\n";
			$return .= "\n";
		}
		
		return $return;
	}
	
	/**
	 * generates an PHP file header
	 *
	 * @param string		$namespace
	 * @return string
	 */
	public function header( $namespace = null )
	{
		return $this->add( "<?php ".( $namespace ? 'namespace '.$namespace.';' : '' ) );
	}
	
	/**
	 * generates an PHP comment
	 *
	 * @param string		$str
	 * @param int		$wordwrap
	 * @return string
	 */
	public function comment( $str, $wordwrap = 80 )
	{
		$str = trim( wordwrap( $str, $wordwrap ) );
		$str = str_replace( "\n", "\n".' * ', $str );
		$str = str_replace( "\n".' * *', "\n".' **', $str );
		return $this->add( "/**\n * ".$str."\n */" );
	}
	
	/**
	 * generates an closure
	 *
	 * @param string		$str
	 * @param int		$wordwrap
	 * @return string
	 */
	public function closure( $name, $content = null, $comment )
	{
		return $this->add( ( $comment ? static::make( 'comment', array( $comment ) )."\n" : '' ).
			$name."\n{\n".str_replace( "\n", "\n\t", "\t".CCStr::capture( $content ) )."\n}" );
	}
	
	/**
	 * generates an PHP class
	 *
	 * @param string		$str
	 * @param int		$wordwrap
	 * @return string
	 */
	public function a_class( $name, $content, $extends = null, $implements = null )
	{
		return $this->add( $this->closure( "class ".$name.
			( $extends ? ' extends '.$extends : '' ).
			( $implements ? ' implements '.$implements : '' ), $content ) );
	}
	
	/**
	 * generates a class property
	 *
	 * @param string		$str
	 * @param int		$wordwrap
	 * @return string
	 */
	public function property( $name, $default = null, $comment = null, $export = true )
	{
		if ( $default !== null )
		{
			if ( $export )
			{
				$default = var_export( $default, true );
			}
		} 
		
		return $this->add( ( $comment ? static::make( 'comment', array( $comment ) )."\n" : '' ).
			$name.( $default !== null ? ' = '. $default : '' ).';' );
	}
}