<?php
/**
 * Bootstrap
 *
 * This file contains initialization code run immediately after system startup.
 * Setup i18n and l10n handling, configure system, prepare event hooks.
 *
 * @package		MicroMVC
 * @author		David Pennington
 * @copyright	(c) 2010 MicroMVC Framework
 * @license		http://micromvc.com/license
 ********************************** 80 Columns *********************************
 */

// System Start Time
define('START_TIME', microtime(true));

// System Start Memory
define('START_MEMORY_USAGE', memory_get_usage());

// Extension of all PHP files
define('EXT', '.php');

// Directory separator (Unix-Style works on all OS)
define('DS', '/');

// Absolute path to the system folder
define('SP', realpath(__DIR__). DS);

// Is this an AJAX request?
define('AJAX_REQUEST', strtolower(getenv('HTTP_X_REQUESTED_WITH')) === 'xmlhttprequest');

// The current TLD address, scheme, and port
define('DOMAIN', (strtolower(getenv('HTTPS')) == 'on' ? 'https' : 'http') . '://'
	. getenv('HTTP_HOST') . (($p = getenv('SERVER_PORT')) != 80 AND $p != 443 ? ":$p" : ''));

// The current site path
define('PATH', parse_url(getenv('REQUEST_URI'), PHP_URL_PATH));

require(SP . 'vendor/autoload' . EXT);

// Include common system functions
require(SP . 'Common' . EXT);

\Micro\View::$directory = SP . 'View/';

\Micro\Cookie::$settings = config()->cookie;

// Register events
foreach(config()->events as $event => $class)
{
	event($event, NULL, $class);
}

/*
if(preg_match_all('/[\-a-z]{2,}/i', getenv('HTTP_ACCEPT_LANGUAGE'), $locales))
{
	$locales = $locales[0];
}
*/

// Get locale from user agent
if(isset($_COOKIE['lang']))
{
	$preference = $_COOKIE['lang'];
}
else
{
	$preference = Locale::acceptFromHttp(getenv('HTTP_ACCEPT_LANGUAGE'));
}

// Match preferred language to those available, defaulting to generic English
$locale = Locale::lookup(config()->languages, $preference, false, 'en');

// Default Locale
Locale::setDefault($locale);
setlocale(LC_ALL, $locale . '.utf-8');
//putenv("LC_ALL", $locale);

// Default timezone of server
date_default_timezone_set('UTC');

// iconv encoding
iconv_set_encoding("internal_encoding", "UTF-8");

// multibyte encoding
mb_internal_encoding('UTF-8');

// Enable global error handling
set_error_handler(array('\Micro\Error', 'handler'));
register_shutdown_function(array('\Micro\Error', 'fatal'));

