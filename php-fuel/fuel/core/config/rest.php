<?php
/**
 * Part of the Fuel framework.
 *
 * @package    Fuel
 * @version    1.5
 * @author     Fuel Development Team
 * @license    MIT License
 * @copyright  2010 - 2013 Fuel Development Team
 * @link       http://fuelphp.com
 */

/**
 * NOTICE:
 *
 * If you need to make modifications to the default configuration, copy
 * this file to your app/config folder, and make them in there.
 *
 * This will allow you to upgrade fuel without losing your custom config.
 */


return array(

	/*
	| What format should the data be returned in by default?
	|
	|	Default: xml
	|
	*/
	'default_format' => 'xml',

	/*
	| XML Basenode name
	|
	|	Default: xml
	|
	*/
	'xml_basenode' => 'xml',

	/*
	| Name for the password protected REST API displayed on login dialogs
	|
	|	E.g: My Secret REST API
	|
	*/
	'realm' => 'REST API',

	/*
	| Is login required and if so, which type of login?
	|
	|	'' = no login required, 'basic' = unsecure login, 'digest' = more secure login
	|
	*/
	'auth' => '',

	/*
	| array of usernames and passwords for login
	|
	|	array('admin' => '1234')
	|
	*/
	'valid_logins' => array('admin' => '1234'),

	/*
	| Ignore HTTP_ACCEPT
	|
	| A lot of work can go into detecting incoming data,
	| disabling this will speed up your requests if you do not use a ACCEPT header.
	|
	*/
	'ignore_http_accept' => false,

);


