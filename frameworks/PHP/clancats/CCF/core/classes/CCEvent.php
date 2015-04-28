<?php namespace Core;
/**
 * Le ClanCats Event Handler
 *
 * @package 		ClanCats-Framework
 * @author     		Mario Döring <mariodoering@me.com>
 * @version 		0.4
 * @copyright 		2010 - 2013 ClanCats GmbH 
 *
 */
class CCEvent {
	
	/*
	 * the events
	 */
	protected static $events = array();
	
	/**
	 * mind an event ( event registery )
	 *
	 * @param string	$event
	 * @param mixed		$callback
	 * @return void
	 */
	public static function mind( $event, $callback ) {
		static::$events[$event]['callbacks'][] = $callback;
	}
	
	/**
	 * register an event that should be called before another one
	 *
	 * @param string	$event
	 * @param mixed		$callback
	 * @return void
	 */
	public static function before( $event, $callback ) {
		static::$events[$event]['before'][] = $callback;
	}
	
	/**
	 * register an event that should be called after another one
	 *
	 * @param string	$event
	 * @param mixed		$callback
	 * @return void
	 */
	public static function after( $event, $callback ) {
		static::$events[$event]['after'][] = $callback;
	}
	
	/**
	 * clear an event 
	 *
	 * @param string	$event
	 * @param mixed		$callback
	 * @return void
	 */
	public static function clear( $event, $what = null ) {
		if ( is_null( $what ) ) {
			unset( static::$events[$event] ); return;
		}
		
		if ( array_key_exists( $what, static::$events[$event][$what] ) ) {
			unset( static::$events[$event][$what] );
		}
	}
	
	/** 
	 * also fires an event but returns a bool positiv 
	 * only if all responses where positiv, like all system ready YESS!
	 *
	 * @param string	$event
	 * @param array		$params
	 * @return array
	 */
	public static function ready( $event, $params = array() ) {
		$responses = static::fire( $event, $params );
		
		foreach( static::pack( $responses ) as $response ) {
			if ( $response !== true ) {
				return false;
			}
		}
		
		return true;
	}
	
	/**
	 * fire an event
	 *
	 * @param string	$event
	 * @param array		$params
	 * @return array
	 */
	public static function fire( $event, $params = array() ) {
		
		if ( !array_key_exists( $event, static::$events ) ) {
			return;
		}
		
		$event = static::$events[$event];
		$return = array();
		
		if ( array_key_exists( 'before', $event ) ) {
			$return['before'] = static::call( $event['before'], $params );
		}
		
		if ( array_key_exists( 'callbacks', $event ) ) {
			$return['main'] = static::call( $event['callbacks'], $params );
		}
		
		if ( array_key_exists( 'after', $event ) ) {
			$return['after'] = static::call( $event['after'], $params );
		}
		
		return static::pack( $return );
	}
	
	/**
	 * pass an var to an event
	 * the diffrence to fire is that the param gets modified by each event
	 * 
	 * @param string	$event
	 * @param mixed		$param
	 * @return mixed
	 */
	public static function pass( $event, $param = null ) {
		
		if ( !array_key_exists( $event, static::$events ) ) {
			return $param;
		}
		
		$event = static::$events[$event];
		
		if ( array_key_exists( 'before', $event ) ) {
			$param = static::call( $event['before'], $param, true );
		}
		
		if ( array_key_exists( 'callbacks', $event ) ) {
			$param = static::call( $event['callbacks'], $param, true );
		}
		
		if ( array_key_exists( 'after', $event ) ) {
			$param = static::call( $event['after'], $param, true );
		}
		
		return $param;
	}
	
	/**
	 * call an callback array
	 * 
	 * @param array		$callbacks
	 * @param array		$params
	 * @param bool		$pass
	 * @retrun array
	 */
	protected static function call( $callbacks, $params, $pass = false ) {
		
		$response = array();
		
		if ( $pass ) {
			$response = $params;
		}
		
		foreach ( $callbacks as $callback ) {
			if ( $pass ) {
				$response = call_user_func_array( $callback, array( $response ) );
			}
			else {
				$response[] = call_user_func_array( $callback, $params );
			}
		}
		
		return $response;
	}
	
	/**
	 * packs all responses into one array
	 *
	 * @param array		$responses
	 * @return array
	 */
	protected static function pack( $responses ) {
		return array_merge( CCArr::get( 'before', $responses, array() ), CCArr::get( 'main', $responses, array() ), CCArr::get( 'after', $responses, array() ) );
	}
}