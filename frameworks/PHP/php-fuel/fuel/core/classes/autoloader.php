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
 * The Autloader is responsible for all class loading.  It allows you to define
 * different load paths based on namespaces.  It also lets you set explicit paths
 * for classes to be loaded from.
 *
 * @package     Fuel
 * @subpackage  Core
 */
class Autoloader
{

	/**
	 * @var  array  $classes  holds all the classes and paths
	 */
	protected static $classes = array();

	/**
	 * @var  array  holds all the namespace paths
	 */
	protected static $namespaces = array();

	/**
	 * Holds all the PSR-0 compliant namespaces.  These namespaces should
	 * be loaded according to the PSR-0 standard.
	 *
	 * @var  array
	 */
	protected static $psr_namespaces = array();

	/**
	 * @var  array  list off namespaces of which classes will be aliased to global namespace
	 */
	protected static $core_namespaces = array(
		'Fuel\\Core'
	);

	/**
	 * @var  array  the default path to look in if the class is not in a package
	 */
	protected static $default_path = null;

	/**
	 * @var  bool  whether to initialize a loaded class
	 */
	protected static $auto_initialize = null;

	/**
	 * Adds a namespace search path.  Any class in the given namespace will be
	 * looked for in the given path.
	 *
	 * @param   string  the namespace
	 * @param   string  the path
	 * @return  void
	 */
	public static function add_namespace($namespace, $path, $psr = false)
	{
		static::$namespaces[$namespace] = $path;
		if ($psr)
		{
			static::$psr_namespaces[$namespace] = $path;
		}
	}

	/**
	 * Adds an array of namespace paths. See {add_namespace}.
	 *
	 * @param   array  the namespaces
	 * @param   bool   whether to prepend the namespace to the search path
	 * @return  void
	 */
	public static function add_namespaces(array $namespaces, $prepend = false)
	{
		if ( ! $prepend)
		{
			static::$namespaces = array_merge(static::$namespaces, $namespaces);
		}
		else
		{
			static::$namespaces = $namespaces + static::$namespaces;
		}
	}

	/**
	 * Returns the namespace's path or false when it doesn't exist.
	 *
	 * @param   string      the namespace to get the path for
	 * @return  array|bool  the namespace path or false
	 */
	public static function namespace_path($namespace)
	{
		if ( ! array_key_exists($namespace, static::$namespaces))
		{
			return false;
		}

		return static::$namespaces[$namespace];
	}

	/**
	 * Adds a classes load path.  Any class added here will not be searched for
	 * but explicitly loaded from the path.
	 *
	 * @param   string  the class name
	 * @param   string  the path to the class file
	 * @return  void
	 */
	public static function add_class($class, $path)
	{
		static::$classes[$class] = $path;
	}

	/**
	 * Adds multiple class paths to the load path. See {@see Autoloader::add_class}.
	 *
	 * @param   array  the class names and paths
	 * @return  void
	 */
	public static function add_classes($classes)
	{
		foreach ($classes as $class => $path)
		{
			static::$classes[$class] = $path;
		}
	}

	/**
	 * Aliases the given class into the given Namespace.  By default it will
	 * add it to the global namespace.
	 *
	 * <code>
	 * Autoloader::alias_to_namespace('Foo\\Bar');
	 * Autoloader::alias_to_namespace('Foo\\Bar', '\\Baz');
	 * </code>
	 *
	 * @param  string  $class      the class name
	 * @param  string  $namespace  the namespace to alias to
	 */
	public static function alias_to_namespace($class, $namespace = '')
	{
		empty($namespace) or $namespace = rtrim($namespace, '\\').'\\';
		$parts = explode('\\', $class);
		$root_class = $namespace.array_pop($parts);
		class_alias($class, $root_class);
	}

	/**
	 * Register's the autoloader to the SPL autoload stack.
	 *
	 * @return	void
	 */
	public static function register()
	{
		spl_autoload_register('Autoloader::load', true, true);
	}

	/**
	 * Returns the class with namespace prefix when available
	 *
	 * @param	string
	 * @return	bool|string
	 */
	protected static function find_core_class($class)
	{
		foreach (static::$core_namespaces as $ns)
		{
			if (array_key_exists($ns_class = $ns.'\\'.$class, static::$classes))
			{
				return $ns_class;
			}
		}

		return false;
	}

	/**
	 * Add a namespace for which classes may be used without the namespace prefix and
	 * will be auto-aliased to the global namespace.
	 * Prefixing the classes will overwrite core classes and previously added namespaces.
	 *
	 * @param	string
	 * @param	bool
	 * @return	void
	 */
	public static function add_core_namespace($namespace, $prefix = true)
	{
		if ($prefix)
		{
			array_unshift(static::$core_namespaces, $namespace);
		}
		else
		{
			array_push(static::$core_namespaces, $namespace);
		}
	}

	/**
	 * Loads a class.
	 *
	 * @param   string  $class  Class to load
	 * @return  bool    If it loaded the class
	 */
	public static function load($class)
	{
		// deal with funny is_callable('static::classname') side-effect
		if (strpos($class, 'static::') === 0)
		{
			// is called from within the class, so it's already loaded
			return true;
		}

		$loaded = false;
		$class = ltrim($class, '\\');
		$namespaced = ($pos = strripos($class, '\\')) !== false;

		if (empty(static::$auto_initialize))
		{
			static::$auto_initialize = $class;
		}

		if (isset(static::$classes[$class]))
		{
			include str_replace('/', DS, static::$classes[$class]);
			static::init_class($class);
			$loaded = true;
		}
		elseif ($full_class = static::find_core_class($class))
		{
			if ( ! class_exists($full_class, false) and ! interface_exists($full_class, false))
			{
				include static::prep_path(static::$classes[$full_class]);
			}
			class_alias($full_class, $class);
			static::init_class($class);
			$loaded = true;
		}
		else
		{
			$full_ns = substr($class, 0, $pos);

			if ($full_ns)
			{
				foreach (static::$namespaces as $ns => $path)
				{
					$ns = ltrim($ns, '\\');
					if (stripos($full_ns, $ns) === 0)
					{
						$path .= static::class_to_path(
							substr($class, strlen($ns) + 1),
							array_key_exists($ns, static::$psr_namespaces)
						);
						if (is_file($path))
						{
							require $path;
							static::init_class($class);
							$loaded = true;
							break;
						}
					}
				}
			}

			if ( ! $loaded)
			{
				$path = APPPATH.'classes/'.static::class_to_path($class);

				if (file_exists($path))
				{
					include $path;
					static::init_class($class);
					$loaded = true;
				}
			}
		}

		// Prevent failed load from keeping other classes from initializing
		if (static::$auto_initialize == $class)
		{
			static::$auto_initialize = null;
		}

		return $loaded;
	}

	/**
	 * Reset the auto initialize state after an autoloader exception.
	 * This method is called by the exception handler, and is considered an
	 * internal method!
	 *
	 * @access protected
	 */
	public static function _reset()
	{
		static::$auto_initialize = null;
	}

	/**
	 * Takes a class name and turns it into a path.  It follows the PSR-0
	 * standard, except for makes the entire path lower case, unless you
	 * tell it otherwise.
	 *
	 * Note: This does not check if the file exists...just gets the path
	 *
	 * @param   string  $class  Class name
	 * @param   bool    $psr    Whether this is a PSR-0 compliant class
	 * @return  string  Path for the class
	 */
	protected static function class_to_path($class, $psr = false)
	{
		$file  = '';
		if ($last_ns_pos = strripos($class, '\\'))
		{
			$namespace = substr($class, 0, $last_ns_pos);
			$class = substr($class, $last_ns_pos + 1);
			$file = str_replace('\\', DS, $namespace).DS;
		}
		$file .= str_replace('_', DS, $class).'.php';

		if ( ! $psr)
		{
			$file = strtolower($file);
		}

		return $file;
	}

	/**
	 * Prepares a given path by making sure the directory separators are correct.
	 *
	 * @param   string  $path  Path to prepare
	 * @return  string  Prepped path
	 */
	protected static function prep_path($path)
	{
		return str_replace(array('/', '\\'), DS, $path);
	}

	/**
	 * Checks to see if the given class has a static _init() method.  If so then
	 * it calls it.
	 *
	 * @param	string	the class name
	 */
	protected static function init_class($class)
	{
		if (static::$auto_initialize === $class)
		{
			static::$auto_initialize = null;
			if (method_exists($class, '_init') and is_callable($class.'::_init'))
			{
				call_user_func($class.'::_init');
			}
		}
	}
}
