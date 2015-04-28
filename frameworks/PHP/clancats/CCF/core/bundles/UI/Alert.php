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
class Alert
{
	/**
	 * notification holder
	 * 
	 * @var array
	 */
	protected static $_alerts = array();
	
	/**
	 * available alert types
	 *
	 * @var array
	 */
	protected static $_types = array(
		'danger',
		'warning',
		'info',
		'success',
	);
	
	/**
	 * When loading the alerts we going to add alerts
	 * from the previous request stored in the session
	 *
	 * @return void
	 */
	public static function _init() 
	{
		if ( \CCSession::has( 'ui.alerts' ) ) 
		{
			static::$_alerts = \CCSession::once( 'ui.alerts', array() );
		}
	}
	
	/**
	 * Validate the alert type and format the message
	 *
	 * @param string 			$type
	 * @param string|array 		$message
	 * @return array
	 */
	private static function prepare( $type, $message )
	{
		// to avoid typos and other mistakes we 
		// validate the alert type
		$type = strtolower( $type );
		if ( !in_array( $type, static::$_types ) ) 
		{
			throw new Exception( "UI\Alert - Unknown alert type '{$type}'!" );
		}
		
		// We always need to return an array
		if ( !is_array( $message ) )
		{
			return array( $message );
		}
		return $message;
	}
	
	/**
	 * Flash an alert
	 * This will add the alert to session so it gets displayed on the next request.
	 *
	 * @param string 			$type
	 * @param string|array 		$message
	 * @return void
	 */
	public static function flash( $type, $message ) 
	{	
		\CCSession::add( 'ui.alerts.'.$type, static::prepare( $type, $message ) );
	}
	
	/**
	 * Add an alert to the current queue
	 *
	 * @param string 			$type
	 * @param string|array 		$message
	 * @return void
	 */
	public static function add( $type, $message ) 
	{
		static::$_alerts[$type][] = static::prepare( $type, $message );
	}
	
	/**
	 * Render the alerts and reset the queue
	 *
	 * @return UI\HTML
	 */
	public static function render()
	{
		$html = Builder::handle( 'alert', static::$_alerts );
		
		static::$_alerts = array();
		
		return $html;
	}
}