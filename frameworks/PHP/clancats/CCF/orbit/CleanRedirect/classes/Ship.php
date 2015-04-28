<?php namespace CleanRedirect;
/**
 * CleanRedirect Ship
 *
 * @package		CleanRedirect
 * @author		Mario Döring <Mario Döring>
 * @version		1.0.0
 */
class Ship extends \CCOrbit_Ship 
{
	/**
	 * initialize the ship
	 */
	public function wake() 
	{
		$uri = \CCServer::server('REQUEST_URI', '/' );
		
		// fix doubled slashes
		$uri_fixed = preg_replace( '/(\/+)/','/', $uri );
		
		// redirect if not match
		if ( $uri != $uri_fixed ) 
		{
			\CCRedirect::to( $uri_fixed )->send( true ); die;
		}
	}
}
