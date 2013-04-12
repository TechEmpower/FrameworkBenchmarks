<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

/**
 * This console front-controller file is the gateway to your application
 * through the command line.  It is responsible for intercepting requests, and
 * handing them off to the `Dispatcher` for processing.
 *
 * Determine if we're in an application context by moving up the directory tree
 * looking for a `config` directory with a `bootstrap.php` file in it.  If no
 * application context is found, just boot up the core framework.
 */
$params = getopt("", array("app::"));
$working = $params ? array_pop($params) : getcwd();
$app = null;

/**
 * If we're not running inside an application (i.e. a self-bootstrapping library), bootstrap the
 * core automatically with the default settings.
 */
$bootstrap = function() use ($working) {
	define('LITHIUM_LIBRARY_PATH', dirname(dirname(__DIR__)));
	define('LITHIUM_APP_PATH', $working);

	if (!include LITHIUM_LIBRARY_PATH . '/lithium/core/Libraries.php') {
		$message  = "Lithium core could not be found.  Check the value of LITHIUM_LIBRARY_PATH in ";
		$message .= __FILE__ . ".  It should point to the directory containing your ";
		$message .= "/libraries directory.";
		throw new ErrorException($message);
	}

	$resources = sys_get_temp_dir();
	$templates = $resources . '/tmp/cache/templates/';
	if (!is_dir($templates)) {
		mkdir($resources . '/tmp/cache/templates/', 0777, true);
	}

	lithium\core\Libraries::add('lithium');
	lithium\core\Libraries::add(basename($working), array(
		'default' => true,
		'path' => $working,
		'resources' => $resources
	));
};

/**
 * The following will dispatch the request and exit with the status code as
 * provided by the `Response` object returned from `run()`.
 *
 * The following will instantiate a new `Request` object and pass it off to the
 * `Dispatcher` class.  By default, the `Request` will automatically aggregate
 * all the server / environment settings, and request content (i.e. options and
 * arguments passed to the command) information.
 *
 * The `Request` is then used by the `Dispatcher` (in conjunction with the
 * `Router`) to determine the correct command to dispatch to. The response
 * information is then encapsulated in a `Response` object, which is returned
 * from the command to the `Dispatcher`.
 *
 * The `Response` object will contain information about the status code which
 * is used as the exit code when ending the execution of this script and
 * returned to the callee.
 *
 * @see lithium\console\Request
 * @see lithium\console\Response
 * @see lithium\console\Dispatcher
 * @see lithium\console\Router
 */
$run = function() {
	return lithium\console\Dispatcher::run(new lithium\console\Request())->status;
};

/**
 * Look to see if there's a bootstrap file. If there is, this is either a Lithium application or
 * plugin.
 */
if (file_exists("{$working}/config/bootstrap.php")) {
	$app = $working;
} elseif (file_exists("{$working}/app/config/bootstrap.php")) {
	$app = "{$working}/app";
}

/**
 * Attempt to bootstrap the application and execute the request. On failure, use the default
 * bootstrap.
 */
if ($app) {
	foreach (array("bootstrap.php", "bootstrap/libraries.php") as $file) {
		if (!file_exists($path = "{$app}/config/{$file}")) {
			continue;
		}
		if (preg_match("/^define\([\"']LITHIUM_LIBRARY_PATH[\"']/m", file_get_contents($path))) {
			include "{$app}/config/bootstrap.php";
			exit($run());
		}
	}
}

/**
 * We're not running inside a Lithium application. Use the default bootstrap and execute the
 * request.
 */
$bootstrap();
$app ? include "{$app}/config/bootstrap.php" : null;
exit($run());

?>