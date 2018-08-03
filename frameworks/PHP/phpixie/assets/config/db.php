<?php

return array(
	'default' => array(
		'user'=>'benchmarkdbuser',
		'password' => 'benchmarkdbpass',
		'driver' => 'PDO',
		
		//'Connection' is required if you use the PDO driver
		'connection'=>'mysql:host=tfb-database;dbname=hello_world',
		
		// 'db' and 'host' are required if you use Mysqli driver
		'db' => 'hello_world',
		'host'=>'tfb-database',
		'connectionOptions' => array(
			PDO::ATTR_PERSISTENT => true
		)
	)
);
