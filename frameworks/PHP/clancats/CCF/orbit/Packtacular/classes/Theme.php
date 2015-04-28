<?php namespace Packtacular;
/**
 * Theme
 **
 * 
 * @package		Packtacular
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class Theme extends \CCTheme
{
	/**
	 * the theme configuration
	 *
	 * @var CCConfig
	 */
	public static $config = null;
	
	/**
	 * static theme init
	 *
	 * @return void
	 */
	public static function _init()
	{
		static::$config = \CCConfig::create( static::view_namespace().'::theme' );
	}
	
	/**
	 * Returns the current view namespace 
	 * You need to implement this method in your theme subclass.
	 *
	 * @return string
	 */
	public static function view_namespace()
	{
		return __NAMESPACE__;	
	}
	
	/**
	 * This the current public namespace
	 * By default its simply the PUBLICPATH/assets/<theme_namespace>
	 * You need to implement this method in your theme subclass.
	 *
	 * @return string
	 */
	public static function public_namespace()
	{
		return "assets/".__NAMESPACE__.'/';
	}
	
	/**
	 * custom theme render stuff
	 *
	 * @param string		$file
	 * @return string
	 */
	public function render( $file = null ) 
	{
		foreach( static::$config->default as $key => $value )
		{
			if ( !$this->has( $key ) )
			{
				$this->set( $key, $value );
			}
		}
		
		// assign the topic to the title
		if ( $this->has( 'topic' ) && $this->has( 'title' ) )
		{
			$this->title = sprintf( $this->get( 'title' ), $this->get( 'topic' ) );
		}
		
		// add assets from config
		foreach( static::$config->assets as $asset => $container ) 
		{	
			$container = explode( "@", $container );
			
			if ( isset( $container[1] ) ) 
			{
				\CCAsset::pack( $asset, $container[0], $container[1] );
			}
			else 
			{
				\CCAsset::add( $asset, $container[0] );
			}	
		}
		
		return parent::render( $file );
	}
}
