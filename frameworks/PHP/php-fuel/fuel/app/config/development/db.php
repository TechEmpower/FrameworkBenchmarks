<?php
/**
 * The development database settings. These get merged with the global settings.
 */

return array(
	'default' => array(
		'connection'  => array(
            'dsn'        => 'mysql:host=192.168.100.102;dbname=hello_world',
            'username'   => 'benchmarkdbuser',
            'password'   => 'benchmarkdbpass',
		),
	),
);
