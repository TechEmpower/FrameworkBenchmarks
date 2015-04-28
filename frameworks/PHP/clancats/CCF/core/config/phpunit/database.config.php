<?php 
/*
 *---------------------------------------------------------------
 * Database configuration for phpunit
 *---------------------------------------------------------------
 */
return array(
	
	'main' => 'app',
	
	'app' => array(
		// selected database
		'db'	 => 'ccf2_phpunit_application',
	
		// driver
		'driver' => 'mysql',
	
		// auth
		'host'		=> '127.0.0.1',
		'user' 		=> 'root',
		'pass'		=> '',
		'charset'	=> 'utf8'
	),
	
	'phpunit' => array(
		// selected database
		'db'	 => 'ccf2_phpunit_database',

		// driver
		'driver' => 'mysql',

		// auth
		'host'		=> '127.0.0.1',
		'user' 		=> 'root',
		'pass'		=> '',
		'charset'	=> 'utf8'
	),
	
	'phpunit_sqlite' => array(

		// driver
		'driver' => 'sqlite',
		'path' => CCPath::get( 'CCUnit::test.db' ),

		'charset'	=> 'utf8'
	),
);