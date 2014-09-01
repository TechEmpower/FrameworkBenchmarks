<?php namespace UI;
/**
 * Uiify component Builder
 *
 * @package 		Uiify
 * @author     	Mario Döring <mario@clancats.com>
 * @version 		1.0
 * @copyright 	2013 ClanCats GmbH
 *
 */
class Builder 
{
	/**
	 * The current Bulder object
	 *
	 * @var UI\Builder_Interface
	 */
	protected static $builder = null;
	
	/**
	 * The user interface configuration
	 *
	 * @var UI\Builder_Interface
	 */
	public static $config = null;
	
	/**
	 * Static init
	 *
	 * @return void
	 */
	public static function _init()
	{
		// select the current builder
		$builder_class = \ClanCats::$config->get( 'ui.builder', "\\UI\\Builder_Bootstrap" );
		static::$builder = new $builder_class;
		
		// load the ui configuration
		static::$config = \CCConfig::create( 'ui' );
	}
	
	/** 
	 * Handle a build request
	 *
	 * @param string			$key
	 * @param mixed ...
	 */
	public static function handle()
	{
		$args = func_get_args();
		$key = array_shift( $args );
		
		return call_user_func_array( array( static::$builder, 'build_'.$key ), $args );
	}
}