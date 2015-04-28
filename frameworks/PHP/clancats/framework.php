<?php
/**
 *---------------------------------------------------------------
 * Framework initialisation
 *---------------------------------------------------------------
 *
 * This is the framework initialisation. Thats the point where
 * all important parts come together and build something 
 * aweomse together.
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 * ###
 *
 *---------------------------------------------------------------
 * Application root 
 *---------------------------------------------------------------
 * 
 * The application root or CCROOT defines the absoulte path to 
 * the framework.
 */
define( 'CCROOT', __DIR__.'/' );

/*
 *---------------------------------------------------------------
 * file extension 
 *---------------------------------------------------------------
 * 
 * This defines the global used file extention of the php files.
 */
define( 'EXT', '.php' );

/*
 *---------------------------------------------------------------
 * get the boot paths
 *---------------------------------------------------------------
 * 
 * You can modify that file, its yours. Its especially useful
 * if you have multiple installations on one server and want 
 * to use just one core or one orbit for them all.
 */
$paths = require CCROOT.'boot/paths'.EXT;

/*
 *---------------------------------------------------------------
 * the direcotries
 *---------------------------------------------------------------
 * 
 * Here are the module directories defined. 
 * @ToDo: move them to the classes that use that direcotries. 
 *        that way the you could subclass a class and define 
 *        a custom direcotry.
 */
$directories = array(
	'controller'			=> 'controllers/',
	'language'			=> 'language/',
	'class'				=> 'classes/',
	'console'			=> 'console/',
	'config'				=> 'config/',
	'view'				=> 'views/',
	'test'				=> 'tests/',
);

/*
 *---------------------------------------------------------------
 * wake CCF
 *---------------------------------------------------------------
 * 
 * Lets require the ClanCatsFramework resources
 */
require $paths['core'].'wake'.EXT;

/*
 *---------------------------------------------------------------
 * wake the application
 *---------------------------------------------------------------
 * 
 * Lets wake the main application.
 */
ClanCats::wake_app( 'App' );

// at this point the app has completet its own boot
CCProfiler::check( "CCF - App completed." );