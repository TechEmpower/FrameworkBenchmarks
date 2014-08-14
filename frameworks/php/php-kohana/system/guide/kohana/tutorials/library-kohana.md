# Importing Kohana as a Library

If you're working with an existing codebase it's often difficult to modernise the code as it would mean a complete rewrite and there's rarely the time. An alternative is to improve the codebase incrementally as best you can, gradually outsourcing code to external libraries to reduce the amount of old code there is to maintain.

This tutorial describes how to include the Kohana PHP framework into existing PHP applications, without having to use the routing and HMVC request handling features.

[!!] The code modified in this tutorial was copied from Kohana version 3.1.x. You may need to update it to work with future releases.

In normal usage of the Kohana framework, the `index.php` file acts as the request handler; it sets up the environment, loads the system configuration, and then handles the request (see [Request Flow](flow)).
We'll walk you through the steps required to create a file we'll call `include.php` which will allow you to include Kohana from exiting PHP applications.

## Demo application

The following file will serve as our (insultingly simple) demo application for this tutorial.

### File: `demo.php`

~~~
	<?php
		$content = 'Hello World';
	?>
	<html>
	<head>
		<title>Demo page</title>
	</head>
	<body>
		<?php echo $content; ?>
	</body>
	</html>
~~~

## Install Kohana

[Download and install the Kohana framework](install); from this point on, we'll be referring to the location of the Kohana libraries as the `kohana` directory.

## Create a common setup file

Since `index.php` and `include.php` will duplicate a lot of code, we're going to move that code to a third file, `common.php`. The bulk of the code is unchanged; we've changed the install check to exit rather than return after rendering, and removed the request execution.

The new file creates the initial request object, rather than fully executing the request, so that, if you do define routes, the `Request::$initial` variable will be set up correctly.

### File: `kohana/common.php`

~~~
	<?php

	/**
	 * The directory in which your application specific resources are located.
	 * The application directory must contain the bootstrap.php file.
	 *
	 * @link http://kohanaframework.org/guide/about.install#application
	 */
	$application = 'application';

	/**
	 * The directory in which your modules are located.
	 *
	 * @link http://kohanaframework.org/guide/about.install#modules
	 */
	$modules = 'modules';

	/**
	 * The directory in which the Kohana resources are located. The system
	 * directory must contain the classes/kohana.php file.
	 *
	 * @link http://kohanaframework.org/guide/about.install#system
	 */
	$system = 'system';

	/**
	 * The default extension of resource files. If you change this, all resources
	 * must be renamed to use the new extension.
	 *
	 * @link http://kohanaframework.org/guide/about.install#ext
	 */
	define('EXT', '.php');

	/**
	 * Set the PHP error reporting level. If you set this in php.ini, you remove this.
	 * @link http://www.php.net/manual/errorfunc.configuration#ini.error-reporting
	 *
	 * When developing your application, it is highly recommended to enable notices
	 * and strict warnings. Enable them by using: E_ALL | E_STRICT
	 *
	 * In a production environment, it is safe to ignore notices and strict warnings.
	 * Disable them by using: E_ALL ^ E_NOTICE
	 *
	 * When using a legacy application with PHP >= 5.3, it is recommended to disable
	 * deprecated notices. Disable with: E_ALL & ~E_DEPRECATED
	 */
	error_reporting(E_ALL | E_STRICT);

	/**
	 * End of standard configuration! Changing any of the code below should only be
	 * attempted by those with a working knowledge of Kohana internals.
	 *
	 * @link http://kohanaframework.org/guide/using.configuration
	 */

	// Set the full path to the docroot
	define('DOCROOT', realpath(dirname(__FILE__)).DIRECTORY_SEPARATOR);

	// Make the application relative to the docroot, for symlink'd index.php
	if ( ! is_dir($application) AND is_dir(DOCROOT.$application))
		$application = DOCROOT.$application;

	// Make the modules relative to the docroot, for symlink'd index.php
	if ( ! is_dir($modules) AND is_dir(DOCROOT.$modules))
		$modules = DOCROOT.$modules;

	// Make the system relative to the docroot, for symlink'd index.php
	if ( ! is_dir($system) AND is_dir(DOCROOT.$system))
		$system = DOCROOT.$system;

	// Define the absolute paths for configured directories
	define('APPPATH', realpath($application).DIRECTORY_SEPARATOR);
	define('MODPATH', realpath($modules).DIRECTORY_SEPARATOR);
	define('SYSPATH', realpath($system).DIRECTORY_SEPARATOR);

	// Clean up the configuration vars
	unset($application, $modules, $system);

	if (file_exists('install'.EXT))
	{
		// Load the installation check
		include 'install'.EXT;
		exit; // Changes were made here
	}

	/**
	 * Define the start time of the application, used for profiling.
	 */
	if ( ! defined('KOHANA_START_TIME'))
	{
		define('KOHANA_START_TIME', microtime(TRUE));
	}

	/**
	 * Define the memory usage at the start of the application, used for profiling.
	 */
	if ( ! defined('KOHANA_START_MEMORY'))
	{
		define('KOHANA_START_MEMORY', memory_get_usage());
	}

	// Bootstrap the application
	require APPPATH.'bootstrap'.EXT;

	/**
	 * Instantiate the request object. A source of the URI can be passed, eg: $_SERVER['PATH_INFO'].
	 * If no source is specified, the URI will be automatically detected.
	 */
	Request::factory(); // Changes were made here
~~~

## Alter Kohana's `index.php`

Having moved most of the code from Kohana's `index.php` to `common.php` the new `kohana/index.php` contains only this:

### File: `kohana/index.php`

~~~
	<?php

	require_once 'common.php';

	// Execute the request
	Request::$initial->execute()
		->execute()
		->send_headers(TRUE)
		->body();
~~~

## Create the include file

Our `include.php` file is also pretty simple. The try-catch clause is needed because if the request matches no routes Kohana will throw an `HTTP_Exception_404` exception.

### File: `kohana/include.php`

~~~
	<?php

	try {
		require_once 'common.php';
	}
	catch (HTTP_Exception_404 $e)
	{
		// The request did not match any routes; ignore the 404 exception.
	}
~~~

**NB:** Due to the way Kohana's routing  works, if the request matches no routes it will fail to instantiate an object, and `Request::$current` and `Request::$initial` will not be available.

## Integration

Now that we're set up, we can add Kohana into our application using a single include, and then we're good to go.

### File: `demo.php`

~~~
	<?php
		require_once 'kohana/include.php';

		$content = 'Hello World';
		$content = HTML::anchor('http://kohanaframework.org/', $content);
	?>
	<html>
	<head>
		<title>Demo page</title>
	</head>
	<body>
		<?php echo $content; ?>
		<hr />
		<?php echo URL::base(); ?>
		<hr />
		<?php echo Debug::dump(array(1,2,3,4,5)); ?>
	</body>
	</html>
~~~