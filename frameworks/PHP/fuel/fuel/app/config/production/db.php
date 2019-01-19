<?php
/**
 * The production database settings. These get merged with the global settings.
 */

return array(
	'default' => array(
		'connection'  => array(
			'dsn'        => 'mysql:host=tfb-database;dbname=hello_world',
			'username'   => 'benchmarkdbuser',
			'password'   => 'benchmarkdbpass',
			'persistent' => true,
		),
	),
);
