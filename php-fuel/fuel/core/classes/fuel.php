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

namespace Fuel\Core;

/**
 * General Fuel Exception class
 */
class FuelException extends \Exception {}

/**
 * The core of the framework.
 *
 * @package		Fuel
 * @subpackage	Core
 */
class Fuel
{

	/**
	 * @var  string  The version of Fuel
	 */
	const VERSION = '1.5-dev';

	/**
	 * @var  string  constant used for when in testing mode
	 */
	const TEST = 'test';

	/**
	 * @var  string  constant used for when in development
	 */
	const DEVELOPMENT = 'development';

	/**
	 * @var  string  constant used for when in production
	 */
	const PRODUCTION = 'production';

	/**
	 * @var  string  constant used for when testing the app in a staging env.
	 */
	const STAGE = 'stage';

	/**
	 * @var  int  No logging
	 */
	const L_NONE = 0;

	/**
	 * @var  int  Log everything
	 */
	const L_ALL = 99;

	/**
	 * @var  int  Log debug massages and below
	 */
	const L_DEBUG = 100;

	/**
	 * @var  int  Log info massages and below
	 */
	const L_INFO = 200;

	/**
	 * @var  int  Log warning massages and below
	 */
	const L_WARNING = 300;

	/**
	 * @var  int  Log errors only
	 */
	const L_ERROR = 400;

	/**
	 * @var  bool  Whether Fuel has been initialized
	 */
	public static $initialized = false;

	/**
	 * @var  string  The Fuel environment
	 */
	public static $env = \Fuel::DEVELOPMENT;

	/**
	 * @var  bool  Whether to display the profiling information
	 */
	public static $profiling = false;

	public static $locale = 'en_US';

	public static $timezone = 'UTC';

	public static $encoding = 'UTF-8';

	public static $is_cli = false;

	public static $is_test = false;

	public static $volatile_paths = array();

	protected static $_paths = array();

	protected static $packages = array();

	final private function __construct() { }

	/**
	 * Initializes the framework.  This can only be called once.
	 *
	 * @access	public
	 * @return	void
	 */
	public static function init($config)
	{
		if (static::$initialized)
		{
			throw new \FuelException("You can't initialize Fuel more than once.");
		}

		static::$_paths = array(APPPATH, COREPATH);

		// Is Fuel running on the command line?
		static::$is_cli = (bool) defined('STDIN');

		\Config::load($config);

		// Start up output buffering
		ob_start(\Config::get('ob_callback', null));

		if (\Config::get('caching', false))
		{
			\Finder::instance()->read_cache('FuelFileFinder');
		}

		static::$profiling = \Config::get('profiling', false);
		static::$profiling and \Profiler::init();

		// set a default timezone if one is defined
		static::$timezone = \Config::get('default_timezone') ?: date_default_timezone_get();
		date_default_timezone_set(static::$timezone);

		static::$encoding = \Config::get('encoding', static::$encoding);
		MBSTRING and mb_internal_encoding(static::$encoding);

		static::$locale = \Config::get('locale', static::$locale);

		if ( ! static::$is_cli)
		{
			if (\Config::get('base_url') === null)
			{
				\Config::set('base_url', static::generate_base_url());
			}
		}

		// Run Input Filtering
		\Security::clean_input();

		\Event::register('shutdown', 'Fuel::finish');

		// Always load classes, config & language set in always_load.php config
		static::always_load();

		// Load in the routes
		\Config::load('routes', true);
		\Router::add(\Config::get('routes'));

		// Set locale, log warning when it fails
		if (static::$locale)
		{
			setlocale(LC_ALL, static::$locale) or
				logger(\Fuel::L_WARNING, 'The configured locale '.static::$locale.' is not installed on your system.', __METHOD__);
		}

		static::$initialized = true;

		// fire any app created events
		\Event::instance()->has_events('app_created') and \Event::instance()->trigger('app_created', '', 'none');

		if (static::$profiling)
		{
			\Profiler::mark(__METHOD__.' End');
		}
	}

	/**
	 * Cleans up Fuel execution, ends the output buffering, and outputs the
	 * buffer contents.
	 *
	 * @access	public
	 * @return	void
	 */
	public static function finish()
	{
		if (\Config::get('caching', false))
		{
			\Finder::instance()->write_cache('FuelFileFinder');
		}

		if (static::$profiling)
		{
			// Grab the output buffer and flush it, we will rebuffer later
			$output = ob_get_clean();

			$headers = headers_list();
			$show = true;

			foreach ($headers as $header)
			{
				if (stripos($header, 'content-type') === 0 and stripos($header, 'text/html') === false)
				{
					$show = false;
				}
			}

			if ($show)
			{
				\Profiler::mark('End of Fuel Execution');
				if (preg_match("|</body>.*?</html>|is", $output))
				{
					$output  = preg_replace("|</body>.*?</html>|is", '', $output);
					$output .= \Profiler::output();
					$output .= '</body></html>';
				}
				else
				{
					$output .= \Profiler::output();
				}
			}
			// Restart the output buffer and send the new output
			ob_start();
			echo $output;
		}
	}

	/**
	 * Generates a base url.
	 *
	 * @return  string  the base url
	 */
	protected static function generate_base_url()
	{
		$base_url = '';
		if(\Input::server('http_host'))
		{
			$base_url .= \Input::protocol().'://'.\Input::server('http_host');
		}
		if (\Input::server('script_name'))
		{
			$base_url .= str_replace('\\', '/', dirname(\Input::server('script_name')));
		}

		// Add a slash if it is missing and return it
		return rtrim($base_url, '/').'/';
	}

	/**
	 * Includes the given file and returns the results.
	 *
	 * @param   string  the path to the file
	 * @return  mixed   the results of the include
	 */
	public static function load($file)
	{
		return include $file;
	}

	/**
	 * Always load packages, modules, classes, config & language files set in always_load.php config
	 *
	 * @param  array  what to autoload
	 */
	public static function always_load($array = null)
	{
		is_null($array) and	$array = \Config::get('always_load', array());

		isset($array['packages']) and \Package::load($array['packages']);

		isset($array['modules']) and \Module::load($array['modules']);

		if (isset($array['classes']))
		{
			foreach ($array['classes'] as $class)
			{
				if ( ! class_exists($class = ucfirst($class)))
				{
					throw new \FuelException('Always load class does not exist. Unable to load: '.$class);
				}
			}
		}

		/**
		 * Config and Lang must be either just the filename, example: array(filename)
		 * or the filename as key and the group as value, example: array(filename => some_group)
		 */

		if (isset($array['config']))
		{
			foreach ($array['config'] as $config => $config_group)
			{
				\Config::load((is_int($config) ? $config_group : $config), (is_int($config) ? true : $config_group));
			}
		}

		if (isset($array['language']))
		{
			foreach ($array['language'] as $lang => $lang_group)
			{
				\Lang::load((is_int($lang) ? $lang_group : $lang), (is_int($lang) ? true : $lang_group));
			}
		}
	}

	/**
	 * Takes a value and checks if it is a Closure or not, if it is it
	 * will return the result of the closure, if not, it will simply return the
	 * value.
	 *
	 * @param   mixed  $var  The value to get
	 * @return  mixed
	 */
	public static function value($var)
	{
		return ($var instanceof \Closure) ? $var() : $var;
	}

	/**
	 * Cleans a file path so that it does not contain absolute file paths.
	 *
	 * @param   string  the filepath
	 * @return  string  the clean path
	 */
	public static function clean_path($path)
	{
		static $search = array(APPPATH, COREPATH, PKGPATH, DOCROOT, '\\');
		static $replace = array('APPPATH/', 'COREPATH/', 'PKGPATH/', 'DOCROOT/', '/');
		return str_ireplace($search, $replace, $path);
	}
}
