<?php namespace Core;
/**
 * Route
 * The route object that contains information about the calling
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCRoute {
	
	/**
	 * static route factory
	 * 
	 * @param callback		$callback
	 * @param array			$params
	 * @return CCRoute
	 */
	public static function factory( $callback, $params = array() ) {
		$route = new static();
		$route->callback = $callback;
		$route->params = $params;
		return $route;
	}
	
	/**
	 * route constructor
	 * @return CCRoute
	 */
	public function __construct( $uri = null ) {
		$this->uri = $uri;
	}
	
	/*
	 * the callback
	 */
	public $callback = null;
	
	/*
	 * the Uri
	 */
	public $uri = null;
	
	/*
	 * the action
	 */
	public $action = null;
	
	/*
	 * the params
	 */
	public $params = array();
}