<?php namespace CCConsole; use CCCli;
/**
 * Phpunit cmd
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class session extends \CCConsoleController 
{
	/**
	 * return an array with information about the script
	 */
	public function help() 
	{
		return array(
			'name'	=> 'Session utility',
			'desc'	=> '',
			'actions'	=> array(
				'gc'	 => 'Removes old sessions',
			),
		);
	}
	
	/**
	 * Get a manager instance
	 *
	 * @param array 				$params
	 * @return Session\Manager
	 */
	public function get_manager( $params )
	{
		if ( array_key_exists( 'm', $params ) )
		{
			$manager = $params['m'];
		}
		else 
		{
			$manager = reset( $params );
		}
		
		if ( $manager )
		{
			$this->info( 'Using session manager: '.$manager );
		}
		else
		{
			$manager = null;
			$this->info( 'Using default session manager.' );
		}
		
		return \CCSession::manager( $manager );
	}
	
	/**
	 * Gabrage collect old sessions
	 *
	 * @param array 		$params 
	 */
	public function action_gc( $params ) 
	{
		$manager = $this->get_manager( $params );
		$manager->gc();
		
		$this->success( 'Garbage collection executed successfully' );
	}
}