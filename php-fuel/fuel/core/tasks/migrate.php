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

namespace Fuel\Tasks;

/**
 * migrate task
 *
 * use this command line task to deploy and rollback changes
 */
class Migrate
{
	/**
	 * @var	boolean	if true, migrate the app
	 */
	protected static $default = true;

	/**
	 * @var	array	list of modules to migrate
	 */
	protected static $modules = array();

	/**
	 * @var	array	list of packages to migrate
	 */
	protected static $packages = array();

	/**
	 * @var	int	number of modules migrated
	 */
	protected static $module_count = 0;

	/**
	 * @var	int	number of packages migrated
	 */
	protected static $package_count = 0;

	/**
	 * sets the properties by grabbing Cli options
	 */
	public function __construct()
	{
		// load config
		\Config::load('migrations', true);

		// get Cli options
		$modules = \Cli::option('modules', \Cli::option('m'));
		$packages = \Cli::option('packages', \Cli::option('p'));
		$default = \Cli::option('default');
		$all = \Cli::option('all');

		if ($all)
		{
			$modules = true;
			$packages = true;
			$default = true;
		}

		// if modules option set
		if ( ! empty($modules))
		{
			// if true - get all modules
			if ($modules === true)
			{
				// loop through module paths
				foreach (\Config::get('module_paths') as $path)
				{
					// get all modules that have files in the migration folder
					foreach (glob($path . '*/') as $m)
					{
						if (count(glob($m.\Config::get('migrations.folder').'/*.php')))
						{
							static::$modules[] = basename($m);
						}
					}
				}
			}
			// else do selected modules
			else
			{
				static::$modules = explode(',', $modules);
			}
		}

		// if packages option set
		if ( ! empty($packages))
		{
			// if true - get all packages
			if ($packages === true)
			{
				// get all packages that have files in the migration folder
				foreach (\Config::get('package_paths', array(PKGPATH)) as $p)
				{
					foreach (glob($p . '*/') as $pp)
					{
						if (count(glob($pp.\Config::get('migrations.folder').'/*.php')))
						{
							static::$packages[] = basename($pp);
						}
					}
				}
			}
			// else do selected packages
			else
			{
				static::$packages = explode(',', $packages);
			}
		}

		// if packages or modules are specified, and the app isn't, disable app migrations
		if ( ( ! empty($packages) or ! empty($modules)) and empty($default))
		{
			static::$default = false;
		}

		// set the module and package count
		static::$module_count = count(static::$modules);
		static::$package_count = count(static::$packages);
	}

	/**
	 * catches requested method call and runs as needed
	 *
	 * @param string	name of the method to run
	 * @param string	any additional method arguments (not used here!)
	 */
	public function __call($name, $args)
	{
		// set method name
		$name = '_'.$name;

		// make sure the called name exists
		if ( ! method_exists(get_called_class(), $name))
		{
			return static::help();
		}

		// run app (default) migrations if default is true
		if (static::$default)
		{
			static::$name('default', 'app');
		}

		// run migrations on all specified modules
		foreach (static::$modules as $module)
		{
			static::$name($module, 'module');
		}

		// run migrations on all specified packages
		foreach (static::$packages as $package)
		{
			static::$name($package, 'package');
		}
	}

	/**
	 * migrates to the latest version unless -version is specified
	 *
	 * @param string	name of the type (in case of app, it's 'default')
	 * @param string	type (app, module or package)
	 * @param string	direction of migration (up or down)
	 */
	protected static function _run($name, $type)
	{
		// -v or --version
		$version = \Cli::option('v', \Cli::option('version', ''));

		// version is used as a flag, so show it
		if ($version === true)
		{
			\Cli::write('Currently installed migrations for '.$type.':'.$name.':', 'green');

			foreach (\Config::get('migrations.version.'.$type.'.'.$name, array()) as $version)
			{
				\Cli::write('- '.$version);
			}
			return;
		}

		// version contains a timestamp of sorts
		elseif ($version !== '')
		{
			// if version has a value, make sure only 1 item was passed
			if (static::$default + static::$module_count + static::$package_count > 1)
			{
				\Cli::write('Migration: version only accepts 1 item.');
				return;
			}
			$migrations = \Migrate::version($version, $name, $type, \Cli::option('catchup', false));
		}

		// migrate to the latest version
		else
		{
			$migrations = \Migrate::latest($name, $type, \Cli::option('catchup', false));
		}

		// any migrations executed?
		if ($migrations)
		{
			\Cli::write('Performed migrations for '.$type.':'.$name.':', 'green');

			foreach ($migrations as $migration)
			{
				\Cli::write($migration);
			}
		}
		else
		{
			if ($version !== '')
			{
				\Cli::write('No migrations were found for '.$type.':'.$name.'.');
			}
			else
			{
				\Cli::write('Already on the latest migration for '.$type.':'.$name.'.');
			}
		}
	}

	/**
	 * migrates item to current config version
	 *
	 * @param string	name of the type (in case of app, it's 'default')
	 * @param string	type (app, module or package)
	 */
	protected static function _current($name, $type)
	{
		// -v or --version
		if (\Cli::option('v', \Cli::option('version', '')) !== '')
		{
			\Cli::write('You can not define a version when using the "current" command.', 'red');
		}

		$migrations = \Migrate::current($name, $type);

		if ($migrations)
		{
			\Cli::write('Newly installed migrations for '.$type.':'.$name.':', 'green');
			foreach ($migrations as $migration)
			{
				\Cli::write('- '.$migration);
			}
		}
		else
		{
			// migration is already on current version
			\Cli::write('Already on the current migration version for '.$type.':'.$name.'.');
		}
	}

	/**
	 * migrates item up to the given version
	 *
	 * @param string
	 * @param string
	 */
	protected static function _up($name, $type)
	{
		// -v or --version
		$version = \Cli::option('v', \Cli::option('version', null));

		// if version has a value, make sure only 1 item was passed
		if ($version and (static::$default + static::$module_count + static::$package_count > 1))
		{
			\Cli::write('Migration: version only accepts 1 item.');
			return;
		}

		$migrations = \Migrate::up($version, $name, $type);

		if ($migrations)
		{
			\Cli::write('Newly installed migrations for '.$type.':'.$name.':', 'green');
			foreach ($migrations as $migration)
			{
				\Cli::write('- '.$migration);
			}
		}
		else
		{
			// there is no 'up'...
			\Cli::write('You are already on the latest migration version for '.$type.':'.$name.'.');
		}
	}

	/**
	 * migrates item down to the given version
	 *
	 * @param string
	 * @param string
	 */
	protected static function _down($name, $type)
	{
		// -v or --version
		$version = \Cli::option('v', \Cli::option('version', null));

		// if version has a value, make sure only 1 item was passed
		if ($version and (static::$default + static::$module_count + static::$package_count > 1))
		{
			\Cli::write('Migration: version only accepts 1 item.');
			return;
		}

		$migrations = \Migrate::down($version, $name, $type);

		if ($migrations)
		{
			\Cli::write('Reverted migrations for '.$type.':'.$name.':', 'green');
			foreach ($migrations as $migration)
			{
				\Cli::write('- '.$migration);
			}
		}
		else
		{
			// there is no 'down'...
			\Cli::write('There are no migrations installed to revert for '.$type.':'.$name.'.');
		}
	}

	/**
	 * Shows basic help instructions for using migrate in oil
	 */
	public static function help()
	{
		echo <<<HELP
Usage:
    php oil refine migrate[:command] [--version=X]

Fuel commands:
    help     shows this text
    current  migrates to the version defined in the migration configuration file
    up       migrate up to the next version
    down     migrate down to the previous version
    run      run all migrations (default)

Fuel options:
    -v, [--version]  # Migrate to a specific version ( only 1 item at a time)
    --catchup        # Use if you have out-of-sequence migrations that can be safely run

    # The following disable default migrations unless you add --default to the command
    --default                               # re-enables default migration
    --modules -m                            # Migrates all modules
    --modules=item1,item2 -m=item1,item2    # Migrates specific modules
    --packages -p                           # Migrates all packages
    --packages=item1,item2 -p=item1,item2   # Migrates specific modules
    --all                                   # shortcut for --modules --packages --default

Description:
    The migrate task can run migrations. You can go up, down or by default go to the current migration marked in the config file.

Examples:
    php oil r migrate
    php oil r migrate:current
    php oil r migrate:up -v=6
    php oil r migrate:down
    php oil r migrate --version=201203171206
    php oil r migrate --modules --packages --default
    php oil r migrate:up --modules=module1,module2 --packages=package1
    php oil r migrate --modules=module1 -v=3
    php oil r migrate --all

HELP;

	}

}
