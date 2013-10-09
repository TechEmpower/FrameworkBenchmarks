<?php

return array(
	'default' => array(
		'user'=>'benchmarkdbuser',
		'password' => 'benchmarkdbpass',
		'driver' => 'mysql',
		
		//'Connection' is required if you use the PDO driver
		'connection'=>'mysql:host=172.16.98.120;dbname=hello_world',
		
		// 'db' and 'host' are required if you use Mysqli driver
		'db' => 'hello_world',
		'host'=>'172.16.98.120'
	)
);
