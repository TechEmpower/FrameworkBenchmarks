<?php namespace Core;
/**
 * CCView builder
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCView_Builder 
{
	/**
	 * Registerd view builders
	 *
	 * @var array
	 */
	protected static $_view_builders = array(
		'view' => "Core\\CCView_Builder_CCFTemplate",
	);
	
	/**
	 * Register a new view Builder
	 *
	 * @param string 		$name
	 * @param string 		$class
	 * @return void
	 */
	public static function register( $name, $class )
	{
		static::$_view_builders[$name] = $class;
	}
	
	/**
	 * Compile the view
	 *
	 * @param string 		$builder
	 * @param string 		$view_file
	 */
	public static function compile( $builder, $view_file )
	{
		if ( !isset( static::$_view_builders[$builder] ) )
		{
			throw new CCException( "CCView_Builder - view builder '".$builder."' is not registerd." );
		}
		
		$builder = static::$_view_builders[$builder];
		
		if ( !class_exists( $builder ) )
		{
			throw new CCException( "CCView_Builder - invalid view builder '".$builder."'." );
		}
		
		$builder = new $builder( CCFile::read( $view_file ) );
		return $builder->compile();
	}
}