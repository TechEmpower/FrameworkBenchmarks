<?php namespace CC\Core; 
/**
 * Database routes for static namespaces
 *
 * @package 		ClanCats-Framework
 * @author     		Mario Döring <mariodoering@me.com>
 * @version 		0.5
 * @copyright 		2010 - 2013 ClanCats GmbH 
 *
 */
class CCDBRoute {
	
	/**
	 * register a database route
	 *
	 * @param string	$namespace
	 * @param string	$route
	 * @return void
	 */
	public static function register( $namespace, $route ) {
		DB::insert( ClanCats::$config->get( 'router.routes_table' ), array(
			'namespace' => $namespace,
			'route'		=> $route,
			'created_at'=> time(),
		))->run( ClanCats::$config->get( 'router.routes_db' ) );
	}
	
	/**
	 * get the data of a namespace
	 *
	 * @param string	$namespace
	 * @return bool
	 */
	public static function find( $namespace ) {
		return DB::find( 
			ClanCats::$config->get( 'router.routes_table' ),
			$namespace,
			'namespace',
			ClanCats::$config->get( 'router.routes_db' )
		);
	}
	
	/**
	 * check if a namespace is already taken
	 *
	 * @param string	$namespace
	 * @return bool
	 */
	public static function check( $namespace ) {
		return is_null( static::find( $namespace ) );
	}
	
	/**
	 * remove a namespace
	 *
	 * @param string	$namespace
	 * @return bool
	 */
	public static function remove( $namespace ) {
		return DB::delete( 
			ClanCats::$config->get( 'router.routes_table' ),
			$namespace,
			'namespace'
		)->run( ClanCats::$config->get( 'router.routes_db' ) );
	}
}