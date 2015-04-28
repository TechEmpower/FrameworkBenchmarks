<?php namespace Core;
/**
 * ClanCats Shipyard
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCShipyard 
{
	/**
	 * Static call wrapper to require the right builder
	 *
	 * @param string 		$name
	 * @param array 			$arguments
	 * @return CCShipyard_Builder
	 */
	public static function create()
	{
		$arguments = func_get_args();
		
		$name = array_shift( $arguments );
		
		$class_name = "Core\\CCShipyard_".ucfirst( $name );
		
		if ( !class_exists( $class_name ) )
		{
			throw new CCException( "CCShipyard - invalid builder (".$name.") given." );
		}
		
		$builder = new $class_name;
		
		call_user_func_array( array( $builder, 'create' ), $arguments );
		
		return $builder;
	}
}