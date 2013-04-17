<?php
/**
 * Config
 *
 * Core system configuration file
 *
 * @package		MicroMVC
 * @author		David Pennington
 * @copyright	(c) 2010 MicroMVC Framework
 * @license		http://micromvc.com/license
 ********************************** 80 Columns *********************************
 */

// Base site url - Not currently supported!
$config['site_url'] = '/';

// Enable debug mode?
$config['debug_mode'] = FALSE;

// Load boostrap file?
$config['bootstrap'] = TRUE;

// Available translations (Array of Locales)
$config['languages'] = array('en');

/**
 * Database
 *
 * This system uses PDO to connect to MySQL, SQLite, or PostgreSQL.
 * Visit http://us3.php.net/manual/en/pdo.drivers.php for more info.
 */
$config['database'] = array(
	'dns' => "mysql:host=localhost;port=3306;dbname=hello_world",
	'username' => 'benchmarkdbuser',
	'password' => 'benchmarkdbpass',
	'params' => array()
);


/**
 * System Events
 */
$config['events'] = array(
	//'pre_controller'	=> 'Class::method',
	//'post_controller'	=> 'Class::method',
);

/**
 * Cookie Handling
 *
 * To insure your cookies are secure, please choose a long, random key!
 * @link http://php.net/setcookie
 */
$config['cookie'] = array(
	'key' => 'very-secret-key',
	'timeout' => time()+(60*60*4), // Ignore submitted cookies older than 4 hours
	'expires' => 0, // Expire on browser close
	'path' => '/',
	'domain' => '',
	'secure' => '',
	'httponly' => '',
);


/**
 * API Keys and Secrets
 *
 * Insert you API keys and other secrets here.
 * Use for Akismet, ReCaptcha, Facebook, and more!
 */

//$config['XXX_api_key'] = '...';

