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
class phpunit extends \CCConsoleController {

	/**
	 * return an array with information about the script
	 */
	public function help() 
	{
		return array(
			'name'	=> 'PHPUnit utility',
			'desc'	=> 'Helps keeping your tests right.',
			'actions'	=> array(
				'build'	=> 'Builds the phpunit.xml file',
			),
		);
	}

	/**
	 * Builds the phpunit.xml file
	 *
	 * @param array 		$params 
	 */
	public function action_build( $params ) 
	{
		$test_directories = array();

		// check if there is an application tests direcotry
		if ( is_dir( APPPATH.CCDIR_TEST ) )
		{
			$this->line( 'found tests in your application.' );
			$test_directories['App'] = APPPATH.CCDIR_TEST;
		}

		// add the core tests
		$test_directories['Core'] = COREPATH.'../'.CCDIR_TEST;

		// check all bundles for tests
		foreach( \CCFinder::$bundles as $bundle => $path )
		{
			if ( is_dir( $path.CCDIR_TEST ) )
			{
				$this->info( 'found tests in the '.$bundle.' bundle.' );
				$test_directories[$bundle] = $path.CCDIR_TEST;
			}
		}

		// we have to remove CCROOT from the paths to get the relative one
		foreach( $test_directories as $key => $dir )
		{
			$test_directories[$key] = './'.str_replace( CCROOT, '', $dir );
		}

		// finally generate an phpunit.xml file
		$xml = new \DOMDocument("1.0", 'UTF-8');

		$root = $xml->createElement( "phpunit" );


		$testsuites = $xml->createElement( "testsuites" );

		foreach( $test_directories as $key => $dir )
		{

			$testsuite = $xml->createElement( "testsuite" );
			$testsuite->setAttribute( 'name', $key.' tests' );

			$directory = $xml->createElement( "directory", $dir );
			$directory->setAttribute( 'suffix', EXT );

			$testsuite->appendChild( $directory );

			$testsuites->appendChild( $testsuite );
		}

		$root->appendChild( $testsuites );
		$root = $xml->appendChild( $root );
		$root->setAttribute( 'colors', 'true' );
		$root->setAttribute( 'bootstrap', 'boot/phpunit.php' );

		$xml->formatOutput = true;

		\CCFile::write( CCROOT.'phpunit.xml', $xml->saveXML() );
	}
}