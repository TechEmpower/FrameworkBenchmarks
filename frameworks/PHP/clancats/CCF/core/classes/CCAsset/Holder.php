<?php namespace Core;
/**
 * This is the Asset holder
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCAsset_Holder
{	
	/**
	 * path modifier
	 *
	 * @var string
	 */
	public $path = 'assets/';
	
	/**
	 * Content
	 *
	 * @var array
	 */
	public $assets = array();
	
	/** 
	 * Add an asset to the holder. Basically this method checks the 
	 * file extension to sort them and generate the correct code using the macros.
	 *
	 *     $holder->add( 'jquery.js' );
	 *     $holder->add( 'style.css' );
	 *     $holder->add( '<script>document.write( "Hello World" );</script>' );
	 *
	 * @param string			$item
	 * @return void
	 */
	public function add( $item ) 
	{
		// when the first character is a "smaller than" we simply assume
		// that a custom tag has been passed and not a filepath
		if ( strpos( $item, "<" ) !== false ) 
		{
			$macro = '_';
		} else {
			$macro = CCStr::extension( $item );
		}
		
		if ( !isset( $this->assets[$macro] ) )
		{
			$this->assets[$macro] = array();
		}
		
		$this->assets[$macro][] = $item;
	}
	
	/**
	 * Get all assets by a specific macro / type
	 *
	 *     $holder->get();
	 *     $holder->get( 'css' );
	 *
	 * @param string			$type		By passing null all items from all types are returned
	 * @return array
	 */
	public function get( $type = null ) 
	{
		if ( is_null( $type ) )
		{
			return $this->assets;
		}
		
		if ( !isset( $this->assets[$type] ) )
		{
			return array();
		}
		
		return $this->assets[$type];
	}
	
	/**
	 * Clear the asset holder, deletes all contained assets 
	 * of a special type or if $type is null everything.
	 *
	 *     $holder->clear();
	 *     $holder->clear( 'js' );
	 * 
	 * @param string 		$type
	 * @return void
	 */
	public function clear( $type = null ) 
	{
		if ( is_null( $type ) )
		{
			$this->assets = array();
		}
		else
		{
			if ( isset( $this->assets[$type] ) )
			{
				$this->assets[$type] = array();
			}
		}
	}
}