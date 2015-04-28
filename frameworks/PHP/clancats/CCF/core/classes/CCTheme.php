<?php namespace Core;
/**
 * Theme object
 * The theme is also just a subclass of an CCView
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCTheme extends CCView 
{	
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
		return "assets/".__NAMESPACE_;
	}
	
	/**
	 * theme creator
	 * returns a new view instance
	 *
	 * @param string		$file
	 * @param array 		$data
	 * @param bool		$encode
	 * @return CCView
	 */
	public static function create( $file = null, $data = null, $encode = false ) 
	{
		$view = parent::create( static::view_namespace().'::'.$file, $data, $encode );
		
		// set the asset holder path
		\CCAsset::holder( 'theme' )->path = static::public_namespace();
		
		// set the vendor path
		\CCAsset::holder( 'vendor' )->path = 'assets/vendor/';
		
		// you may ask why not just return the parent::create(bla bla.. directly?
		// i actually wanted to do something with that var view. So if you'r still
		// able to read this i completly forgot it.
		return $view;
	}
	
	/**
	 * Creates a new view based on the current theme
	 * If the view cannot be found in the theme it's going to use the app::views
	 *
	 * @param string		$file
	 * @param array 		$data
	 * @param bool		$encode
	 * @return CCView
	 */
	public function view( $file = null, $data = null, $encode = false )
	{
		if ( \CCView::exists( $view = static::view_namespace().'::'.$file ) )
		{
			return \CCView::create( $view, $data, $encode );
		}
		
		return \CCView::create( $file, $data, $encode );
	}
}