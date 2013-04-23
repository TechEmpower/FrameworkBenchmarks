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

	/**
	 * Manual browscap parsing configuration.
	 *
	 * This will be used when your PHP installation has no browscap defined
	 * in your php.ini, httpd.conf or .htaccess, and you can't configure one.
	 */
	'browscap' => array(

		/**
		 * Whether of not manual parsing is enabled.
		 *
		 * set to false to disable this functionality.
		 */
		'enabled' => true,

		/**
		 * Location from where the updated browscap file can be downloaded.
		 *
		 * Note: these are temporary links awaiting relaunch of the browscap project
		 */
		'url' => 'http://tempdownloads.browserscap.com/stream.asp?Lite_PHP_BrowsCapINI',  // only major browsers and search engines
		//'url' => 'http://tempdownloads.browserscap.com/stream.asp?Full_PHP_BrowscapINI',  // complete file, approx. 3 times the lite version

		/**
		 * Method used to download the updated browscap file
		 *
		 * 	Default: 'wrapper'
		 *
		 * possible values are: 'local', 'wrapper', 'curl'
		 */
		 'method' => 'wrapper',

		/**
		 * Filename for the local browscap.ini file (for method 'local').
		 *
		 * 	Default: ''
		 */
		 'file' => '/tmp/php_browscap.ini',
	),

	/**
	 * Cache configuration.
	 *
	 * The agent class caches all matched agent strings for future reference
	 * so the browscap file doesn't need to be loaded, as it's quite large.
	 *
	 * Also, the parsed and condensed browscap ini file is stored in cache as
	 * well, so when a new user agent string needs to be looked up, no further
	 * parsing is needed.
	 */
	'cache' => array(

		/**
		 * Storage driver to use to cache agent class entries. If not defined,
		 * the default driver defined in config/cache.php will be used.
		 *
		 * 	Default: ''
		 */
		'driver' => '',

		/**
		 * Cache expiry.
		 *
		 * Number of seconds after which a cached agent result expires.
		 *
		 *	Default: 604800 (every 7 days)
		 *
		 * Note that to prevent abuse of the site publishing the browsecap files,
		 * you can not set the expiry time lower than 7200 (2 hours)
		 */
		'expiry' => 604800,

		/**
		 * Identifier used to store agent class cache elements
		 *
		 *	Default: 'fuel.agent'
		 *
		 */
		'identifier' => 'fuel.agent',
	),

);


