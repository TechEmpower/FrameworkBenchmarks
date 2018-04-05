<?php 
/*
 *---------------------------------------------------------------
 * Database configuration
 *---------------------------------------------------------------
 */
return array(
	/*
	 * the default database
	 */
	'main' =>  array(
		// selected database
		'db'	 => 'hello_world',
	
		// driver
		'driver' => 'mysql',
	
		// auth
		'host'		=> 'tfb-database',
		'user' 		=> 'benchmarkdbuser',
		'pass'		=> 'benchmarkdbpass',
		'charset'	=> 'utf8'
	),
);
