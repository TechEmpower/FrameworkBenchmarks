<?php defined('SYSPATH') or die('No direct access allowed.');

return array
(
    'default' => array
    (
        'type'       => 'PDO',
        'connection' => array(
		/**
		 * The following options are available for PDO:
		 *
		 * string   dsn         Data Source Name
		 * string   username    database username
		 * string   password    database password
		 * boolean  persistent  use persistent connections?
		 */
	    'dsn'        => 'mysql:host=localhost;dbname=hello_world',
            'username'   => 'benchmarkdbuser',
            'password'   => 'benchmarkdbpass',
            'persistent' => FALSE,
        ),
        'table_prefix' => '',
        'charset'      => 'utf8',
        'caching'      => FALSE,
        'profiling'    => TRUE,
    )
);
