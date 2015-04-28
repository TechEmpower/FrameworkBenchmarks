<?php namespace Bootstrap;
/**
 * Bootstrap Theme
 ** 
 *
 * @package		BootstrapTheme
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class Theme extends \Packtacular\Theme
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
		return "assets/".__NAMESPACE__.'/';
	}
}
