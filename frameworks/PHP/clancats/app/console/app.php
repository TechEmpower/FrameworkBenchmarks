<?php namespace CCConsole; use CCCli;
/**
 * App console
 * stuff to maintain your application
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class app extends \CCConsoleController {

	/**
	 * return an array with information about the script
	 */
	public function help() 
	{
		return array(
			'name'	=> 'App',
			'desc'	=> 'stuff to maintain your application',
			'actions'	=> array(
				'info'	=> 'show information about this application.',
			),
		);
	}

	/**
	 * print information about this application
	 *
	 * @param array 		$params 
	 */
	public function action_info( $params ) 
	{
		
		$app = \ClanCats::runtime();
		
		// print the app name
		$this->line( \CCForge_Php::make( 'comment', 
			$app::$name.PHP_EOL.
			"*".PHP_EOL.
			"Running on ClanCatsFramework ".\ClanCats::VERSION.PHP_EOL.
			"© 2010 - ".date('Y')." ClanCats GmbH".PHP_EOL
		), 'cyan' );
		
		// list printer
		$list = function( $array )
		{
			foreach( $array as $key => $value )
			{
				$this->line( $key.': '.CCCli::color( $value, 'green' ) );
			}
		};	
		
		// print info
		$list( array(
			'Runtime Class'		=> $app,
			'CCF Version'		=> \ClanCats::version(),
			'CCF Environment'	=> \ClanCats::environment(),
			'Development env'	=> var_export( \ClanCats::in_development(), true ),
			'File extention'		=> EXT,
			'Core namespace'		=> CCCORE_NAMESPACE,
		));
		
		// paths
		$this->line( PHP_EOL."Paths", 'cyan' );
		$list( \ClanCats::paths() );
		
		// paths
		$this->line( PHP_EOL."Directories", 'cyan' );
		$list( \ClanCats::directories() );
		
		// autoloader
		$this->line( PHP_EOL."Autoloader - namespaces", 'cyan' );
		$list( \CCFinder::$namespaces );
	}
}