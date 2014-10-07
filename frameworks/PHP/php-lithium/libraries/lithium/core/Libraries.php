<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\core;

use RuntimeException;
use lithium\util\String;
use lithium\util\collection\Filters;
use lithium\core\ConfigException;
use lithium\core\ClassNotFoundException;

/**
 * Manages all aspects of class and file location, naming and mapping. Implements auto-loading for
 * the Lithium core, as well as all applications, plugins and vendor libraries registered.
 * Typically, libraries and plugins are registered in `config/bootstrap/libraries.php`.
 *
 * By convention, plugins and vendor libraries are typically located in `app-path/libraries` or
 * `/libraries` (the former may override the latter). By default, `Libraries` will use its own
 * autoloader for all plugins and vendor libraries, but can be configured to use others on a
 * per-library basis.
 *
 * `Libraries` also handles service location. Various _types_ of classes can be defined by name,
 * using _class patterns_, which define conventions for organizing classes, i.e. `'models'`, which
 * maps to `'{:library}\models\{:name}'`, which will find a model class in any registered app,
 * plugin or vendor library that follows that path (namespace) convention. You can find classes by
 * name (see the `locate()` method for more information on class-locating precedence), or find all
 * models in all registered libraries (apps / plugins / vendor libraries, etc). For more information
 * on modifying the default class organization, or defining your own class types, see the `paths()`
 * method.
 *
 * #### Auto-loading classes
 *
 * Lithium defines several rules, conventions and recommendations for naming and organizing classes.
 * The autoloader itself conforms to the [PHP Interoperability Group's draft
 * specification](http://groups.google.com/group/php-standards/web/psr-0-final-proposal). While the
 * autoloader will load any classes conforming to that specification, Lithium itself follows
 * additional constraints, which are also recommended for Lithium applications, libraries and
 * extensions, and are as follows:
 *
 * - Each library must exist in a top-level vendor namespace
 * - Each top-level vendor namespace must define a set of sub-packages, and should not directly
 *   contain classes
 * - Namespace names must be lowercased and under_scored
 * - Class names must be CamelCased
 *
 * Any libraries registered by calling `Libraries::add()` which follow the autoloader's will have
 * their classes automatically loaded when referenced.
 *
 * @see lithium\core\Libraries::add()
 * @see lithium\core\Libraries::locate()
 * @see lithium\core\Libraries::paths()
 */
class Libraries {

	/**
	 * Stores the closures that represent the method filters. They are indexed by method name.
	 *
	 * @var array
	 */
	protected static $_methodFilters = array();

	/**
	 * The list of class libraries registered with the class loader.
	 *
	 * @var array
	 */
	protected static $_configurations = array();

	/**
	 * Contains a cascading list of search path templates, indexed by base object type.
	 *
	 * Used by `Libraries::locate()` to perform service location. This allows new types of
	 * objects (i.e. models, helpers, cache adapters and data sources) to be automatically
	 * 'discovered' when you register a new vendor library or plugin (using `Libraries::add()`).
	 *
	 * Because paths are checked in the order in which they appear, path templates should be
	 * specified from most-specific to least-specific. See the `locate()` method for usage examples.
	 *
	 * @see lithium\core\Libraries::locate()
	 * @see lithium\core\Libraries::paths()
	 * @var array
	 */
	protected static $_paths = array(
		'adapter' => array(
			'{:library}\extensions\adapter\{:namespace}\{:class}\{:name}',
			'{:library}\{:namespace}\{:class}\adapter\{:name}' => array('libraries' => 'lithium')
		),
		'command' => array(
			'{:library}\extensions\command\{:namespace}\{:class}\{:name}',
			'{:library}\console\command\{:namespace}\{:class}\{:name}' => array(
				'libraries' => 'lithium'
			)
		),
		'controllers' => array(
			'{:library}\controllers\{:namespace}\{:class}\{:name}Controller'
		),
		'data' => array(
			'{:library}\extensions\data\{:namespace}\{:class}\{:name}',
			'{:library}\data\{:namespace}\{:class}\adapter\{:name}' => array(
				'libraries' => 'lithium'
			),
			'{:library}\data\{:namespace}\{:class}\{:name}' => array('libraries' => 'lithium'),
			'{:library}\data\{:class}\adapter\{:name}' => array('libraries' => 'lithium')
		),
		'helper' => array(
			'{:library}\extensions\helper\{:name}',
			'{:library}\template\helper\{:name}' => array('libraries' => 'lithium')
		),
		'libraries' => array(
			'{:app}/libraries/{:name}',
			'{:root}/{:name}'
		),
		'models' => array(
			'{:library}\models\{:name}'
		),
		'strategy' => array(
			'{:library}\extensions\strategy\{:namespace}\{:class}\{:name}',
			'{:library}\extensions\strategy\{:class}\{:name}',
			'{:library}\{:namespace}\{:class}\strategy\{:name}' => array('libraries' => 'lithium')
		),
		'socket' => array(
			'{:library}\extensions\net\socket\{:name}',
			'{:library}\extensions\socket\{:name}',
			'{:library}\net\socket\{:name}'
		),
		'test' => array(
			'{:library}\extensions\test\{:namespace}\{:class}\{:name}',
			'{:library}\test\{:namespace}\{:class}\{:name}' => array('libraries' => 'lithium')
		),
		'tests' => array(
			'{:library}\tests\{:namespace}\{:class}\{:name}Test'
		)
	);

	/**
	 * Stores the name of the default library. When adding a library configuration to the
	 * application, if the `'default'` option flag is set to `true`, the name of the library will
	 * be assigned. To retrieve the default library's configuration, use `Libraries::get(true)`.
	 *
	 * @see lithium\core\Libraries::add()
	 * @see lithium\core\Libraries::get()
	 * @var string
	 */
	protected static $_default;

	/**
	 * Holds cached class paths generated and used by `lithium\core\Libraries::load()`.
	 *
	 * @var array
	 * @see lithium\core\Libraries::load()
	 */
	protected static $_cachedPaths = array();

	/**
	 * Holds associations between fully-namespaced class names and file's paths mapped
	 * with `lithium\core\Libraries::map()`.
	 *
	 * @var array
	 * @see lithium\core\Libraries::map()
	 * @see lithium\core\Libraries::unmap()
	 */
	protected static $_map = array();

	/**
	 * Accessor method for the class path templates which `Libraries` uses to look up and load
	 * classes. Using this method, you can define your own types of classes, or modify the default
	 * organization of built-in class types.
	 *
	 * For example, in a queuing application, you can define a class type called `'job'`:
	 * {{{
	 * Libraries::paths(array('job' => '{:library}\extensions\job\{:name}'));
	 * }}}
	 *
	 * Then, any classes you add to the `extensions/job` directory in your application will be
	 * automatically detected when calling `Libraries::locate('job')`. Additionally, any matching
	 * classes in the `extensions/job` directory of any plugin or vendor library you add to your
	 * application will also be detected.
	 *
	 * Supposing you wanted to have the option of further organizing jobs by class type (some jobs
	 * are related to updating caches, others to sending notifications, etc.), you can specify
	 * multiple paths per class type, with varying levels of specificity:
	 * {{{
	 * Libraries::paths(array('job' => array(
	 * 	'{:library}\extensions\job\{:class}\{:name}',
	 * 	'{:library}\extensions\job\{:name}'
	 * )));
	 * }}}
	 *
	 * This allows you to, for example, have two different classes called `Cleanup`. One may be
	 * located in `app\extensions\job\Cleanup`, while the other is in
	 * `app\extensions\job\cache\Cleanup`. Calling: {{{Libraries::locate('job');}}} will find
	 * both classes, while {{{Libraries::locate('job.cache');}}} will only find the second. You can
	 * also find individual jobs by name: {{{Libraries::locate('job', 'Cleanup');}}}
	 *
	 * See `Libraries::locate()` for more information on using built-in and user-defined paths to
	 * look up classes.
	 *
	 * In addition to adding custom class types, `paths()` allows you to redefine the naming and
	 * organization of existing types. For example, if you wished to reference your model classes
	 * as `app\models\PostModel` instead of `app\models\Post`, you can do the following:
	 * {{{Libraries::paths(array('models' => '{:library}\models\{:name}Model'));}}} Note, however,
	 * that this is a destructive, not an additive operation, and will replace any existing paths
	 * defined for that type. If you wish to add a search path for an existing type, you must do
	 * the following:
	 * {{{
	 * $existing = Libraries::paths('controllers');
	 * Libraries::paths(array('controller' => array_merge(
	 * 	array('{:library}\extensions\controllers\{:name}Controller'), (array) $existing
	 * )));
	 * }}}
	 *
	 * @see lithium\core\Libraries::locate()
	 * @see lithium\core\Libraries::$_paths
	 * @param mixed $path If `$path` is a string, returns the path(s) associated with that path
	 *              type, or `null` if no paths are defined for that type.
	 * @return mixed
	 */
	public static function paths($path = null) {
		if (empty($path)) {
			return static::$_paths;
		}
		if (is_string($path)) {
			return isset(static::$_paths[$path]) ? static::$_paths[$path] : null;
		}
		static::$_paths = array_filter(array_merge(static::$_paths, (array) $path));
	}

	/**
	 * Adds a class library from which files can be loaded.
	 *
	 * The `add()` method registers a named library configuration to your application, and is used
	 * to allow the framework to auto-load classes on an as-needed basis.
	 *
	 * ### Adding libraries to your application
	 *
	 * In Lithium, libraries represent the broadest unit of class organization in an application,
	 * and _everything_ is a library; this includes your application, and the Lithium framework
	 * itself. Libraries can also be other frameworks, like Solar, Zend Framework or PEAR, or
	 * Lithium plugins, which are simply libraries that follow the same organizational standards
	 * as Lithium applications.
	 *
	 * By convention, libraries are placed in the `libraries` directory inside your application, or
	 * the root `libraries` directory at the top level of the default distribution (i.e. the one
	 * that contains the `lithium` directory), however, you can change this on a case-by-case basis
	 * using the `'path'` key to specify an absolute path to the library's directory.
	 *
	 * @param string $name Library name, i.e. `'app'`, `'lithium'`, `'pear'` or `'aura'`.
	 * @param array $config Specifies where the library is in the filesystem, and how classes
	 *        should be loaded from it.  Allowed keys are:
	 *        - `'bootstrap'` _mixed_: A file path (relative to `'path'`) to a bootstrap script that
	 *          should be run when the library is added, or `true` to use the default bootstrap
	 *          path, i.e. `config/bootstrap.php`.
	 *        - `'defer'` _boolean_: If `true`, indicates that, when locating classes, this library
	 *          should defer to other libraries in order of preference.
	 *        - `'includePath'` _mixed_: If `true`, appends the absolutely-resolved value of
	 *          `'path'` to the PHP include path. If a string, the value is appended to PHP's.
	 *        - `'loader'`: An auto-loader method associated with the library, if any.
	 *        - `'path'`: The directory containing the library.
	 *        - `'prefix'` _string_: The class prefix this library uses, i.e. `'lithium\'`,
	 *          `'Zend_'` or `'Solar_'`. If the library has no global prefix, set to `false`.
	 *        - `'suffix'` _string_: Gets appended to the end of the file name. For example, most
	 *          libraries end classes in `'.php'`, but some use `'.class.php'`, or `'.inc.php'`.
	 *        - `'transform'` _closure_: Defines a custom way to transform a class name into its
	 *          corresponding file path.  Accepts either an array of two strings which are
	 *          interpreted as the pattern and replacement for a regex, or an anonymous function,
	 *          which receives the class name and library configuration arrays as parameters, and
	 *          returns the full physical file path as output.
	 *        - `'resources'` _string_: If this is the default library, this maybe set to the
	 *          absolute path to the write-enabled application resources directory, which is used
	 *          for caching, log files, uploads, etc.
	 * @return array Returns the resulting set of options created for this library.
	 */
	public static function add($name, array $config = array()) {
		$defaults = array(
			'path' => null,
			'prefix' => $name . "\\",
			'suffix' => '.php',
			'loader' => null,
			'includePath' => false,
			'transform' => null,
			'bootstrap' => true,
			'defer' => false,
			'default' => false
		);
		if ($name === 'lithium') {
			$defaults['defer'] = true;
			$defaults['bootstrap'] = false;
			$defaults['path'] = dirname(__DIR__);
			$defaults['loader'] = 'lithium\core\Libraries::load';
		}
		if (isset($config['default']) && $config['default']) {
			static::$_default = $name;
			$defaults['path'] = LITHIUM_APP_PATH;
			$defaults['bootstrap'] = false;
			$defaults['resources'] = LITHIUM_APP_PATH . '/resources';
		}
		$config += $defaults;

		if (!$config['path']) {
			if (!$config['path'] = static::_locatePath('libraries', compact('name'))) {
				throw new ConfigException("Library `{$name}` not found.");
			}
		}
		$config['path'] = str_replace('\\', '/', $config['path']);
		static::_configure(static::$_configurations[$name] = $config);
		return $config;
	}

	/**
	 * Configures the application environment based on a library's settings, including appending to
	 * the include path, loading a bootstrap file, and registering a loader with SPL's autoloading
	 * system.
	 *
	 * @param array $config The new library's configuration array.
	 * @return void
	 */
	protected static function _configure($config) {
		if ($config['includePath']) {
			$path = ($config['includePath'] === true) ? $config['path'] : $config['includePath'];
			set_include_path(get_include_path() . PATH_SEPARATOR . $path);
		}
		if ($config['bootstrap'] === true) {
			$path = "{$config['path']}/config/bootstrap.php";
			$config['bootstrap'] = file_exists($path) ? 'config/bootstrap.php' : false;
		}
		if ($config['bootstrap']) {
			require "{$config['path']}/{$config['bootstrap']}";
		}
		if (!empty($config['loader'])) {
			spl_autoload_register($config['loader']);
		}
	}

	/**
	 * Allows library information to be retrieved in various ways, including:
	 *
	 * By name:
	 * {{{ embed:lithium\tests\cases\core\LibrariesTest::testLibraryConfigAccess(1-1) }}}
	 *
	 * With no parameters, to return all configuration for all libraries:
	 * {{{ embed:lithium\tests\cases\core\LibrariesTest::testLibraryConfigAccess(22-22) }}}
	 *
	 * By list of names with a key to extract:
	 * {{{ embed:lithium\tests\cases\core\LibrariesTest::testLibraryConfigAccess(34-34) }}}
	 *
	 * With no name, and a key to extract, to return a key/value array, where the library name is
	 * the key, and the `$key` value is the value:
	 * {{{ embed:lithium\tests\cases\core\LibrariesTest::testLibraryConfigAccess(37-37) }}}
	 *
	 * By containing class name:
	 * {{{ embed:lithium\tests\cases\core\LibrariesTest::testLibraryConfigAccess(45-45) }}}
	 *
	 * @param mixed $name Either the name of a library added in `Libraries::add()`, an array of
	 *              library names, or a fully-namespaced class name (see usage examples above).
	 * @param string $key Optional key name. If `$name` is set and is the name of a valid library
	 *               (or an array of valid libraries), returns the given named configuration key,
	 *               i.e. `'path'`, `'webroot'` or `'resources'`.
	 * @return mixed A configuation array for one or more libraries, or a string value if `$key` is
	 *               specified and `$name` is a string, or a library name (string) if `$name` is a
	 *               fully-namespaced class name.
	 */
	public static function get($name = null, $key = null) {
		$configs = static::$_configurations;

		if (!$name && !$key) {
			return $configs;
		}
		if ($name === true) {
			$name = static::$_default;
		}
		if (is_array($name) || (!$name && $key)) {
			$name = $name ?: array_keys(static::$_configurations);
			$call = array(get_called_class(), 'get');
			return array_combine($name, array_map($call, $name, array_fill(0, count($name), $key)));
		}
		$config = isset($configs[$name]) ? $configs[$name] : null;

		if ($key) {
			return isset($config[$key]) ? $config[$key] : null;
		}
		if (strpos($name, '\\') === false) {
			return $config;
		}
		foreach (static::$_configurations as $library => $config) {
			if ($config['prefix'] && strpos($name, $config['prefix']) === 0) {
				return $library;
			}
		}
	}

	/**
	 * Removes a registered library, and unregister's the library's autoloader, if it has one.
	 *
	 * @param mixed $name A string or array of library names indicating the libraries you wish to
	 *        remove, i.e. `'app'` or `'lithium'`. This can also be used to unload plugins by  name.
	 * @return void
	 */
	public static function remove($name) {
		foreach ((array) $name as $library) {
			if (isset(static::$_configurations[$library])) {
				if (static::$_configurations[$library]['loader']) {
					spl_autoload_unregister(static::$_configurations[$library]['loader']);
				}
				unset(static::$_configurations[$library]);
			}
		}
	}

	/**
	 * Finds the classes or namespaces belonging to a particular library. _Note_: This method
	 * assumes loaded class libraries use a consistent class-to-file naming convention.
	 *
	 * @param mixed $library The name of a library added to the application with `Libraries::add()`,
	 *              or `true` to search all libraries.
	 * @param array $options The options this method accepts:
	 *
	 *              - `'path'` _string_: A physical filesystem path relative to the directory of the
	 *                library being searched. If provided, only the classes or namespaces within
	 *                this path will be returned.
	 *              - `'recursive'` _boolean_: If `true`, recursively searches all directories
	 *                (namespaces) in the given library. If `false` (the default), only searches the
	 *                top level of the given path.
	 *              - `'filter'` _string_: A regular expression applied to a class after it is
	 *                transformed into a fully-namespaced class name. The default regular expression
	 *                filters class names based on the
	 *                [PSR-0](http://groups.google.com/group/php-standards/web/psr-0-final-proposal)
	 *                PHP 5.3 naming standard.
	 *              - `'exclude'` _mixed_: Can be either a regular expression of classes/namespaces
	 *                to exclude, or a PHP callable to be used with `array_filter()`.
	 *              - `'namespaces'` _boolean_: Indicates whether namespaces should be included in
	 *                the search results. If `false` (the default), only classes are returned.
	 * @return array Returns an array of fully-namespaced class names found in the given library or
	 *         libraries.
	 * @todo Patch this to skip paths belonging to nested libraries in recursive searches.
	 */
	public static function find($library, array $options = array()) {
		$format = function($file, $config) {
			$trim = array(strlen($config['path']) + 1, strlen($config['suffix']));
			$rTrim = strpos($file, $config['suffix']) !== false ? -$trim[1] : 9999;
			$file = preg_split('/[\/\\\\]/', substr($file, $trim[0], $rTrim));
			return $config['prefix'] . join('\\', $file);
		};

		$defaults = compact('format') + array(
			'path' => '',
			'recursive' => false,
			'filter' => '/^(\w+)?(\\\\[a-z0-9_]+)+\\\\[A-Z][a-zA-Z0-9]+$/',
			'exclude' => '',
			'namespaces' => false
		);
		$options += $defaults;
		$libs = array();

		if ($options['namespaces'] && $options['filter'] === $defaults['filter']) {
			$options['format'] = function($class, $config) use ($format, $defaults) {
				if (is_dir($class)) {
					return $format($class, $config);
				}
				if (preg_match($defaults['filter'], $class = $format($class, $config))) {
					return $class;
				}
			};
			$options['filter'] = false;
		}
		if ($library === true) {
			foreach (static::$_configurations as $library => $config) {
				$libs = array_merge($libs, static::find($library, $options));
			}
			return $libs;
		}
		if (!isset(static::$_configurations[$library])) {
			return null;
		}

		$config = static::$_configurations[$library];
		$options['path'] = "{$config['path']}{$options['path']}/*";
		$libs = static::_search($config, $options);
		return array_values(array_filter($libs));
	}

	/**
	 * Loads the class definition specified by `$class`. Also calls the `__init()` method on the
	 * class, if defined.  Looks through the list of libraries defined in `$_configurations`, which
	 * are added through `lithium\core\Libraries::add()`.
	 *
	 * @see lithium\core\Libraries::add()
	 * @see lithium\core\Libraries::path()
	 * @param string $class The fully-namespaced (where applicable) name of the class to load.
	 * @param boolean $require Specifies whether the class must be loaded or considered an
	 *        exception. Defaults to `false`.
	 * @return void
	 */
	public static function load($class, $require = false) {
		$path = isset(static::$_cachedPaths[$class]) ? static::$_cachedPaths[$class] : null;
		$path = $path ?: static::path($class);

		if ($path && include $path) {
			static::$_cachedPaths[$class] = $path;
			method_exists($class, '__init') ? $class::__init() : null;
		} elseif ($require) {
			throw new RuntimeException("Failed to load class `{$class}` from path `{$path}`.");
		}
	}

	/**
	 * Associtates fully-namespaced class names to their corresponding paths on
	 * the file system.
	 *
	 * Once a class is associtated to a path using `lithium\core\Libraries::map()`
	 * the PSR-0 loader or custom class loader setted using the `transform` or `loader`
	 * option of `lithium\core\Libraries::add()` are ignored and the associtated path
	 * is used instead.
	 *
	 * @param array $classes An array of fully-namespaced class names (as keys) and
	 *        their correponding file's paths (as values).
	 * @return void
	 */
	public static function map(array $classes) {
		foreach ($classes as $key => $value) {
			unset(static::$_cachedPaths[$key]);
		}
		static::$_map = array_merge(static::$_map, $classes);
	}

	/**
	 * Unmap fully-namespaced class names mapped using `lithium\core\Libraries::map()`.
	 *
	 * @see lithium\core\Libraries::map()
	 * @param mixed $classes An array of fully-namespaced class names or
	 *        a string with a fully-namespaced class name.
	 */
	public static function unmap($classes) {
		if (!is_array($classes)) {
			$classes = array($classes);
		}
		foreach ($classes as $value) {
			unset(static::$_map[$value]);
		}
	}

	/**
	 * Get the corresponding physical file path for a class or namespace name.
	 *
	 * @param string $class The class name to locate the physical file for. If `$options['dirs']` is
	 *        set to `true`, `$class` may also be a namespace name, in which case the corresponding
	 *        directory will be located.
	 * @param array $options Options for converting `$class` to a physical path:
	 *        - `'dirs'`: Defaults to `false`. If `true`, will attempt to case-sensitively look up
	 *          directories in addition to files (in which case `$class` is assumed to actually be a
	 *          namespace).
	 * @return string Returns the absolute path to the file containing `$class`, or `null` if the
	 *         file cannot be found.
	 */
	public static function path($class, array $options = array()) {
		$defaults = array('dirs' => false);
		$options += $defaults;
		$class = ltrim($class, '\\');

		if (isset(static::$_cachedPaths[$class]) && !$options['dirs']) {
			return static::$_cachedPaths[$class];
		}
		if (isset(static::$_map[$class]) && !$options['dirs']) {
			return static::$_map[$class];
		}
		foreach (static::$_configurations as $name => $config) {
			$params = $options + $config;
			$suffix = $params['suffix'];

			if ($params['prefix'] && strpos($class, $params['prefix']) !== 0) {
				continue;
			}
			if ($transform = $params['transform']) {
				if ($file = static::_transformPath($transform, $class, $params)) {
					return $file;
				}
				continue;
			}
			$path = str_replace("\\", '/', substr($class, strlen($params['prefix'])));
			$fullPath = "{$params['path']}/{$path}";

			if (!$options['dirs']) {
				return static::$_cachedPaths[$class] = static::realPath($fullPath . $suffix);
			}
			$list = glob(dirname($fullPath) . '/*');
			$list = array_map(function($i) { return str_replace('\\', '/', $i); }, $list);

			if (in_array($fullPath . $suffix, $list)) {
				return static::$_cachedPaths[$class] = static::realPath($fullPath . $suffix);
			}
			return is_dir($fullPath) ? static::realPath($fullPath) : null;
		}
	}

	/**
	 * Wraps the PHP `realpath()` function to add support for finding paths to files inside Phar
	 * archives.
	 *
	 * @param string $path An unresolved path to a file inside a Phar archive which may or may not
	 *               exist.
	 * @return string If `$path` is a valid path to a file inside a Phar archive, returns a string
	 *                in the format `'phar://<path-to-phar>/<path-to-file>'`. Otherwise returns
	 *                `null`.
	 */
	public static function realPath($path) {
		if (($absolutePath = realpath($path)) !== false) {
			return $absolutePath;
		}
		if (!preg_match('%^phar://([^.]+\.phar(?:\.gz)?)(.+)%', $path, $pathComponents)) {
			return;
		}
		list(, $relativePath, $pharPath) = $pathComponents;

		$pharPath = implode('/', array_reduce(explode('/', $pharPath), function ($parts, $value) {
			if ($value === '..') {
				array_pop($parts);
			} elseif ($value !== '.') {
				$parts[] = $value;
			}
			return $parts;
		}));

		if (($resolvedPath = realpath($relativePath)) !== false) {
			if (file_exists($absolutePath = "phar://{$resolvedPath}{$pharPath}")) {
				return $absolutePath;
			}
		}
	}

	/**
	 * Handles the conversion of a class name to a file name using a custom transformation typically
	 * defined in the `'transform'` key of a configuration defined through `Libraries::add()`.
	 *
	 * The transformation can either be a closure which receives two parameters (the class name
	 * as a string, and the library configuration as an array), or an array with two values (one
	 * being the pattern to match, the other being the replacement).
	 *
	 * @see lithium\core\Libraries::add()
	 * @see lithium\core\Libraries::path()
	 * @param mixed $transform Either a closure or an array containing a regular expression match
	 *              and replacement. If the closure returns an empty value, or the regular
	 *              expression fails to match, will return `null`.
	 * @param string $class The class name which is attempting to be mapped to a file.
	 * @param array $options The configuration of the library as passed to `Libraries::add()`, along
	 *              with any options specified in the call to `Libraries::path()`.
	 * @return string Returns transformed path of a class to a file, or `null` if the transformation
	 *         did not match.
	 */
	protected static function _transformPath($transform, $class, array $options = array()) {
		if ((is_callable($transform)) && $file = $transform($class, $options)) {
			return $file;
		}
		if (is_array($transform)) {
			list($match, $replace) = $transform;
			return preg_replace($match, $replace, $class) ?: null;
		}
	}

	/**
	 * Uses service location (i.e. `Libraries::locate()`) to look up a named class of a particular
	 * type, and creates an instance of it, and passes an array of parameters to the constructor.
	 *
	 * If the given class can't be found, an exception is thrown.
	 *
	 * @param string $type The type of class as defined by `Libraries::$_paths`.
	 * @param string $name The un-namespaced name of the class to instantiate.
	 * @param array $options An array of constructor parameters to pass to the class.
	 * @return object If the class is found, returns an instance of it, otherwise throws an
	 *         exception.
	 * @throws lithium\core\ClassNotFoundException Throws an exception if the class can't be found.
	 * @filter
	 */
	public static function instance($type, $name, array $options = array()) {
		$params = compact('type', 'name', 'options');
		$_paths =& static::$_paths;

		$implementation = function($self, $params) use (&$_paths) {
			$name = $params['name'];
			$type = $params['type'];

			if (!$name && !$type) {
				$message = "Invalid class lookup: `\$name` and `\$type` are empty.";
				throw new ClassNotFoundException($message);
			}
			if (!is_string($type) && $type !== null && !isset($_paths[$type])) {
				throw new ClassNotFoundException("Invalid class type `{$type}`.");
			}
			if (!$class = $self::locate($type, $name)) {
				throw new ClassNotFoundException("Class `{$name}` of type `{$type}` not found.");
			}
			if (is_object($class)) {
				return $class;
			}
			if (!(is_string($class) && class_exists($class))) {
				throw new ClassNotFoundException("Class `{$name}` of type `{$type}` not defined.");
			}
			return new $class($params['options']);
		};
		if (!isset(static::$_methodFilters[__FUNCTION__])) {
			return $implementation(get_called_class(), $params);
		}
		$class = get_called_class();
		$method = __FUNCTION__;
		$data = array_merge(static::$_methodFilters[__FUNCTION__], array($implementation));
		return Filters::run($class, $params, compact('data', 'class', 'method'));
	}

	/**
	 * Apply a closure to a method in `Libraries`.
	 *
	 * @see lithium\util\collection\Filters
	 * @param string $method The name of the method to apply the closure to.
	 * @param closure $filter The closure that is used to filter the method.
	 * @return void
	 */
	public static function applyFilter($method, $filter = null) {
		if (!isset(static::$_methodFilters[$method])) {
			static::$_methodFilters[$method] = array();
		}
		static::$_methodFilters[$method][] = $filter;
	}

	/**
	 * Performs service location for an object of a specific type. If `$name` is a string, finds the
	 * first instance of a class with the given name in any registered library (i.e. apps, plugins
	 * or vendor libraries registered via `Libraries::add()`), based on each library's order of
	 * precedence. For example, this will find the first model called `File` in any plugin or class
	 * library loaded into an application, including the application itself.
	 *
	 * {{{Libraries::locate('models', 'File');}}}
	 *
	 * Order of precedence is usually based on the order in which the library was registered (via
	 * `Libraries::add()`), unless the library was registered with the `'defer'` option set to
	 * `true`. All libraries with the `'defer'` option set will be searched in
	 * registration-order **after** searching all libraries **without** `'defer'` set. This means
	 * that in the above example, if an app and a plugin both have a model named `File`, then the
	 * model from the app will be returned first, assuming the app was registered first (and
	 * assuming the default settings).
	 *
	 * If `$name` is not specified, `locate()` returns an array with all classes of the specified
	 * type which can be found. By default, `locate()` searches all registered libraries.
	 *
	 * {{{Libraries::locate('models');}}}
	 *
	 * For example, the above will return an array of all model classes in all registered plugins
	 * and libraries (including the app itself).
	 *
	 * To learn more about adding and modifying the class paths used with `locate()`, see the
	 * documentation for the `paths()` method.
	 *
	 * @see lithium\core\Libraries::paths()
	 * @see lithium\core\Libraries::add()
	 * @see lithium\core\Libraries::_locateDeferred()
	 * @param string $type The type of class to search for. Typically follows the name of the
	 *               directory in which the class is stored, i.e. `'models'`, `'controllers'` or
	 *               `'adapter'`. Some classes types, such as adapters, will require a greater
	 *               degree of specificity when looking up the desired class. In this case, the dot
	 *               syntax is used, as in this example when looking up cache adapters:
	 *               `'adapter.storage.cache'`, or this example, when looking up authentication
	 *               adapters: `'adapter.security.auth'`.
	 * @param string $name The base name (without namespace) of the class you wish to locate. If
	 *               unspecified, `locate()` will attempt to find all classes of the type specified
	 *               in `$type`. If you only wish to search for classes within a single plugin or
	 *               library, you may use the dot syntax to prefix the class name with the library
	 *               name, i.e. `'app.Post'`, which will only look for a `Post` model within the
	 *               app itself.
	 * @param array $options The options to use when searching and returning class names.
	 *              - `'type'` _string_: Defaults to `'class'`. If set to `'file'`, returns file
	 *                names instead of class names.
	 *              - `'library'` _string_: When specified, only the given library/plugin name will
	 *                be searched.
	 * @return mixed If `$name` is specified, returns the name of the first class found that matches
	 *         `$name` and `$type`, or returns `null` if no matching classes were found in any
	 *         registered library. If `$name` is not specified, returns an array of all classes
	 *         found which match `$type`.
	 */
	public static function locate($type, $name = null, array $options = array()) {
		if (is_object($name) || strpos($name, '\\') !== false) {
			return $name;
		}
		$ident = $name ? ($type . '.' . $name) : ($type . '.*');
		$ident .= $options ? '.' . md5(serialize($options)) : null;

		if (isset(static::$_cachedPaths[$ident])) {
			return static::$_cachedPaths[$ident];
		}
		$params = static::_params($type, $name);
		$defaults = array(
			'type' => 'class',
			'library' => $params['library'] !== '*' ? $params['library'] : null
		);
		$options += $defaults;
		unset($params['library']);
		$paths = static::paths($params['type']);

		if (!isset($paths)) {
			return null;
		}
		if ($params['name'] === '*') {
			$result = static::_locateAll($params, $options);
			return (static::$_cachedPaths[$ident] = $result);
		}
		if ($options['library']) {
			$result = static::_locateDeferred(null, $paths, $params, $options);
			return static::$_cachedPaths[$ident] = $result;
		}
		foreach (array(false, true) as $defer) {
			if ($result = static::_locateDeferred($defer, $paths, $params, $options)) {
				return (static::$_cachedPaths[$ident] = $result);
			}
		}
	}

	/**
	 * Returns or sets the the class path cache used for mapping class names to file paths, or
	 * locating classes using `Libraries::locate()`.
	 *
	 * @param array $cache An array of keys and values to use when pre-populating the cache. Keys
	 *              are either class names (which match to file paths as values), or dot-separated
	 *              lookup paths used by `locate()` (which matches to either a single class or an
	 *              array of classes). If `false`, the cache is cleared.
	 * @return array Returns an array of cached class lookups, formatted per the description for
	 *         `$cache`.
	 */
	public static function cache($cache = null) {
		if ($cache === false) {
			static::$_cachedPaths = array();
		}
		if (is_array($cache)) {
			static::$_cachedPaths += $cache;
		}
		return static::$_cachedPaths;
	}

	/**
	 * Performs service location lookups by library, based on the library's `'defer'` flag.
	 * Libraries with `'defer'` set to `true` will be searched last when looking up services.
	 *
	 * @see lithium\core\Libraries::$_paths
	 * @see lithium\core\Libraries::locate()
	 * @param boolean $defer A boolean flag indicating which libraries to search, either the ones
	 *        with the `'defer'` flag set, or the ones without.
	 * @param array $paths List of paths to be searched for the given service (class).  These are
	 *        defined in `lithium\core\Libraries::$_paths`, and are organized by class type.
	 * @param array $params The list of insert parameters to be injected into each path format
	 *        string when searching for classes.
	 * @param array $options
	 * @return string Returns a class path as a string if a given class is found, or null if no
	 *         class in any path matching any of the parameters is located.
	 */
	protected static function _locateDeferred($defer, $paths, $params, array $options = array()) {
		$libraries = static::$_configurations;

		if (isset($options['library'])) {
			$libraries = static::get((array) $options['library']);
		}
		foreach ($libraries as $library => $config) {
			if ($config['defer'] !== $defer && $defer !== null) {
				continue;
			}

			foreach (static::_searchPaths($paths, $library, $params) as $tpl) {
				$params['library'] = rtrim($config['prefix'], '\\');
				$class = str_replace('\\*', '', String::insert($tpl, $params));

				if (file_exists($file = Libraries::path($class, $options))) {
					return ($options['type'] === 'file') ? $file : $class;
				}
			}
		}
	}

	/**
	 * Returns the list of valid search path templates for the given service location lookup.
	 *
	 * @see lithium\core\Libraries::$_paths
	 * @see lithium\core\Libraries::_search()
	 * @param array $paths The list of all possible path templates from `Libraries::$_paths`.
	 * @param string $library The name of the library being searched.
	 * @param array $params The parameters used in the service location lookup.
	 * @return array Returns an array of valid path template strings.
	 */
	protected static function _searchPaths($paths, $library, $params) {
		$result = array();
		$params = array('library' => null, 'type' => null) + $params;

		foreach ($paths as $tpl => $opts) {
			if (is_int($tpl)) {
				$tpl = $opts;
				$opts = array();
			}
			if (isset($opts['libraries']) && !in_array($library, (array) $opts['libraries'])) {
				continue;
			}
			$result[] = $tpl;
		}
		return $result;
	}

	/**
	 * Locates all possible classes for given set of parameters.
	 *
	 * @param array $params
	 * @param array $options
	 * @return array
	 */
	protected static function _locateAll(array $params, array $options = array()) {
		$defaults = array('libraries' => null, 'recursive' => true, 'namespaces' => false);
		$options += $defaults;

		$paths = (array) static::$_paths[$params['type']];
		$libraries = $options['library'] ? $options['library'] : $options['libraries'];
		$libraries = static::get((array) $libraries);
		$flags = array('escape' => '/');
		$classes = array();

		foreach ($libraries as $library => $config) {
			$params['library'] = $config['path'];

			foreach (static::_searchPaths($paths, $library, $params) as $tpl) {
				$options['path'] = str_replace('\\', '/', String::insert($tpl, $params, $flags));
				$options['path'] = str_replace('*/', '', $options['path']);
				$classes = array_merge($classes, static::_search($config, $options));
			}
		}
		return array_unique($classes);
	}

	/**
	 * Helper function for returning known paths given a certain type.
	 *
	 * @see lithium\core\Libraries::$_paths
	 * @param string $type Path type (specified in `Libraries::$_paths`).
	 * @param string $params Path parameters.
	 * @return string Valid path name.
	 */
	protected static function _locatePath($type, $params) {
		if (!isset(static::$_paths[$type])) {
			return;
		}
		$params += array('app' => LITHIUM_APP_PATH, 'root' => LITHIUM_LIBRARY_PATH);

		foreach (static::$_paths[$type] as $path) {
			if (is_dir($path = str_replace('\\', '/', String::insert($path, $params)))) {
				return $path;
			}
		}
	}

	/**
	 * Search file system.
	 *
	 * @param string $config
	 * @param string $options
	 * @param string $name
	 * @return array
	 */
	protected static function _search($config, $options, $name = null) {
		$defaults = array(
			'path' => null,
			'suffix' => null,
			'namespaces' => false,
			'recursive' => false,
			'preFilter' => '/[A-Z][A-Za-z0-9]+\./',
			'filter' => false,
			'exclude' => false,
			'format' => function ($file, $config) {
				$trim = array(strlen($config['path']) + 1, strlen($config['suffix']));
				$file = substr($file, $trim[0], -$trim[1]);
				return $config['prefix'] . str_replace('/', '\\', $file);
			}
		);
		$options += $defaults;
		$path = $options['path'];
		$suffix = $options['namespaces'] ? '' : $config['suffix'];
		$suffix = ($options['suffix'] === null) ? $suffix : $options['suffix'];

		$dFlags = GLOB_ONLYDIR & GLOB_BRACE;
		$libs = (array) glob($path . $suffix, $options['namespaces'] ? $dFlags : GLOB_BRACE);

		if ($options['recursive']) {
			list($current, $match) = explode('/*', $path, 2);
			$dirs = $queue = array_diff((array) glob($current . '/*', $dFlags), $libs);
			$match = str_replace('##', '.+', preg_quote(str_replace('*', '##', $match), '/'));
			$match = '/' . $match . preg_quote($suffix, '/') . '$/';

			while ($queue) {
				if (!is_dir($dir = array_pop($queue))) {
					continue;
				}
				$libs = array_merge($libs, (array) glob("{$dir}/*{$suffix}"));
				$queue = array_merge($queue, array_diff((array) glob("{$dir}/*", $dFlags), $libs));
			}
			$libs = preg_grep($match, $libs);
		}
		if ($suffix) {
			$libs = $options['preFilter'] ? preg_grep($options['preFilter'], $libs) : $libs;
		}
		return static::_filter($libs, (array) $config, $options + compact('name'));
	}

	/**
	 * Filters a list of library search results by the given set of options.
	 *
	 * @param array $libs List of found libraries.
	 * @param array $config The configuration of the library currently being searched within.
	 * @param array $options The options used to filter/format `$libs`.
	 * @return array Returns a copy of `$libs`, filtered and transformed based on the configuration
	 *         provided in `$options`.
	 */
	protected static function _filter($libs, array $config, array $options = array()) {
		if (is_callable($options['format'])) {
			foreach ($libs as $i => $file) {
				$libs[$i] = $options['format']($file, $config);
			}
			$libs = $options['name'] ? preg_grep("/{$options['name']}$/", $libs) : $libs;
		}
		if ($exclude = $options['exclude']) {
			if (is_string($exclude)) {
				$libs = preg_grep($exclude, $libs, PREG_GREP_INVERT);
			} elseif (is_callable($exclude)) {
				$libs = array_values(array_filter($libs, $exclude));
			}
		}
		if ($filter = $options['filter']) {
			if (is_string($filter)) {
				$libs = preg_grep($filter, $libs) ;
			} elseif (is_callable($filter)) {
				$libs = array_filter(array_map($filter, $libs));
			}
		}
		return $libs;
	}

	/**
	 * Get params from type.
	 *
	 * @param string $type
	 * @param string $name default: '*'
	 * @return array type, namespace, class, name
	 */
	protected static function _params($type, $name = "*") {
		if (!$name) {
			$name = '*';
		}
		$library = $namespace = $class = '*';

		if (strpos($type, '.') !== false) {
			$parts = explode('.', $type);
			$type = array_shift($parts);

			switch (count($parts)) {
				case 1:
					list($class) = $parts;
				break;
				case 2:
					list($namespace, $class) = $parts;
				break;
				default:
					$class = array_pop($parts);
					$namespace = join('\\', $parts);
				break;
			}
		}
		if (strpos($name, '.') !== false) {
			$parts = explode('.', $name);
			$library = array_shift($parts);
			$name = array_pop($parts);
			$namespace = $parts ? join('\\', $parts) : "*";
		}
		return compact('library', 'namespace', 'type', 'class', 'name');
	}
}

?>