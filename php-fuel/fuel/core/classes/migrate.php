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
 * Migrate Class
 *
 * @package		Fuel
 * @category	Migrations
 * @link		http://docs.fuelphp.com/classes/migrate.html
 */
class Migrate
{
	/**
	 * @var	array	current migrations registered in the database
	 */
	protected static $migrations = array();

	/**
	 * @var	string	migration classes namespace prefix
	 */
	protected static $prefix = 'Fuel\\Migrations\\';

	/**
	 * @var	string	name of the migration table
	 */
	protected static $table = 'migration';

	/**
	 * @var string  database connection group
	 */
	protected static $connection = null;

	/**
	 * @var	array	migration table schema
	 */
	protected static $table_definition = array(
		'type' => array('type' => 'varchar', 'constraint' => 25),
		'name' => array('type' => 'varchar', 'constraint' => 50),
		'migration' => array('type' => 'varchar', 'constraint' => 100, 'null' => false, 'default' => ''),
	);

	/**
	 * loads in the migrations config file, checks to see if the migrations
	 * table is set in the database (if not, create it), and reads in all of
	 * the versions from the DB.
	 *
	 * @return  void
	 */
	public static function _init()
	{
		logger(\Fuel::L_DEBUG, 'Migrate class initialized');

		// load the migrations config
		\Config::load('migrations', true);

		// set the name of the table containing the installed migrations
		static::$table = \Config::get('migrations.table', static::$table);

		// set the name of the connection group to use
		static::$connection = \Config::get('migrations.connection', static::$connection);

		// installs or upgrades the migration table to the current schema
		static::table_version_check();

		//get all installed migrations from db
		$migrations = \DB::select()
			->from(static::$table)
			->order_by('type', 'ASC')
			->order_by('name', 'ASC')
			->order_by('migration', 'ASC')
			->execute(static::$connection)
			->as_array();

		// convert the db migrations to match the config file structure
		foreach($migrations as $migration)
		{
			isset(static::$migrations[$migration['type']]) or static::$migrations[$migration['type']] = array();
			static::$migrations[$migration['type']][$migration['name']][] = $migration['migration'];
		}
	}

	/**
	 * migrate to a specific version, range of versions, or all
	 *
	 * @param   mixed	version to migrate to (up or down!)
	 * @param   string  name of the package, module or app
	 * @param   string  type of migration (package, module or app)
	 * @param	bool	if true, also run out-of-sequence migrations
	 *
	 * @throws	UnexpectedValueException
	 * @return	array
	 */
	public static function version($version = null, $name = 'default', $type = 'app', $all = false)
	{
		// get the current version from the config
		$all or $current = \Config::get('migrations.version.'.$type.'.'.$name);

		// any migrations defined?
		if ( ! empty($current))
		{
			// get the timestamp of the last installed migration
			if (preg_match('/^(.*?)_(.*)$/', end($current), $match))
			{
				// determine the direction
				$direction = (is_null($version) or $match[1] < $version) ? 'up' : 'down';

				// fetch the migrations
				if ($direction == 'up')
				{
					$migrations = static::find_migrations($name, $type, $match[1], $version);
				}
				else
				{
					$migrations = static::find_migrations($name, $type, $version, $match[1], $direction);

					// we're going down, so reverse the order of mygrations
					$migrations = array_reverse($migrations, true);
				}

				// run migrations from current version to given version
				return static::run($migrations, $name, $type, $direction);
			}
			else
			{
				throw new \UnexpectedValueException('Could not determine a valid version from '.$current.'.');
			}
		}

		// run migrations from the beginning to given version
		return static::run(static::find_migrations($name, $type, null, $version), $name, $type, 'up');
	}

	/**
	 * migrate to a latest version
	 *
	 * @param   string  name of the package, module or app
	 * @param   string  type of migration (package, module or app)
	 * @param	bool	if true, also run out-of-sequence migrations
	 *
	 * @return	array
	 */
	public static function latest($name = 'default', $type = 'app', $all = false)
	{
		// equivalent to from current version (or all) to latest
		return static::version(null, $name, $type, $all);
	}

	/**
	 * migrate to the version defined in the config file
	 *
	 * @param   string  name of the package, module or app
	 * @param   string  type of migration (package, module or app)
	 *
	 * @return	array
	 */
	public static function current($name = 'default', $type = 'app')
	{
		// get the current version from the config
		$current = \Config::get('migrations.version.'.$type.'.'.$name);

		// any migrations defined?
		if ( ! empty($current))
		{
			// get the timestamp of the last installed migration
			if (preg_match('/^(.*?)_(.*)$/', end($current), $match))
			{
				// run migrations from start to current version
				return static::run(static::find_migrations($name, $type, null, $match[1]), $name, $type, 'up');
			}
		}

		// nothing to migrate
		return array();
	}

	/**
	 * migrate up to the next version
	 *
	 * @param   mixed	version to migrate up to
	 * @param   string  name of the package, module or app
	 * @param   string  type of migration (package, module or app)
	 *
	 * @return	array
	 */
	public static function up($version = null, $name = 'default', $type = 'app')
	{
		// get the current version info from the config
		$current = \Config::get('migrations.version.'.$type.'.'.$name);

		// get the last migration installed
		$current = empty($current) ? null : end($current);

		// get the available migrations after the current one
		$migrations = static::find_migrations($name, $type, $current, $version);

		// found any?
		if ( ! empty($migrations))
		{
			// if no version was given, only install the next migration
			is_null($version) and $migrations = array(reset($migrations));

			// install migrations found
			return static::run($migrations, $name, $type, 'up');
		}

		// nothing to migrate
		return array();
	}

	/**
	 * migrate down to the previous version
	 *
	 * @param   mixed	version to migrate down to
	 * @param   string  name of the package, module or app
	 * @param   string  type of migration (package, module or app)
	 *
	 * @return	array
	 */
	public static function down($version = null, $name = 'default', $type = 'app')
	{
		// get the current version info from the config
		$current = \Config::get('migrations.version.'.$type.'.'.$name);

		// any migrations defined?
		if ( ! empty($current))
		{
			// get the last entry
			$current = end($current);

			// get the available migrations before the last current one
			$migrations = static::find_migrations($name, $type, $version, $current, 'down');

			// found any?
			if ( ! empty($migrations))
			{
				// we're going down, so reverse the order of mygrations
				$migrations = array_reverse($migrations, true);

				// if no version was given, only revert the last migration
				is_null($version) and $migrations = array(reset($migrations));

				// revert the installed migrations
				return static::run($migrations, $name, $type, 'down');
			}
		}

		// nothing to migrate
		return array();
	}

	/**
	 * run the action migrations found
	 *
	 * @param   array	list of files to migrate
	 * @param   string  name of the package, module or app
	 * @param   string  type of migration (package, module or app)
	 * @param   string  method to call on the migration
	 *
	 * @return	array
	 */
	protected static function run($migrations, $name, $type, $method = 'up')
	{
		// storage for installed migrations
		$done = array();

		static::$connection === null or \DBUtil::set_connection(static::$connection);

		// Loop through the runnable migrations and run them
		foreach ($migrations as $ver => $migration)
		{
			logger(Fuel::L_INFO, 'Migrating to version: '.$ver);
			$result = call_user_func(array(new $migration['class'], $method));
			if ($result === false)
			{
				logger(Fuel::L_INFO, 'Skipped migration to '.$ver.'.');
				return $done;
			}

			$file = basename($migration['path'], '.php');
			$method == 'up' ? static::write_install($name, $type, $file) : static::write_revert($name, $type, $file);
			$done[] = $file;
		}

		static::$connection === null or \DBUtil::set_connection(null);

		empty($done) or logger(Fuel::L_INFO, 'Migrated to '.$ver.' successfully.');

		return $done;
	}


	/**
	 * add an installed migration to the database
	 *
	 * @param   string  name of the package, module or app
	 * @param   string  type of migration (package, module or app)
	 * @param   string  name of the migration file just run
	 *
	 * @return	void
	 */
	protected static function write_install($name, $type, $file)
	{
		// add the migration just run
		\DB::insert(static::$table)->set(array(
			'name' => $name,
			'type' => $type,
			'migration' => $file,
		))->execute(static::$connection);

		// add the file to the list of run migrations
		static::$migrations[$type][$name][] = $file;

		// make sure the migrations are in the correct order
		sort(static::$migrations[$type][$name]);

		// and save the update to the environment config file
		\Config::set('migrations.version.'.$type.'.'.$name, static::$migrations[$type][$name]);
		\Config::save(\Fuel::$env.DS.'migrations', 'migrations');
	}

	/**
	 * remove a reverted migration from the database
	 *
	 * @param   string  name of the package, module or app
	 * @param   string  type of migration (package, module or app)
	 * @param   string  name of the migration file just run
	 *
	 * @return	void
	 */
	protected static function write_revert($name, $type, $file)
	{
		// remove the migration just run
		\DB::delete(static::$table)
			->where('name', $name)
			->where('type', $type)
			->where('migration', $file)
		->execute(static::$connection);

		// remove the file from the list of run migrations
		if (($key = array_search($file, static::$migrations[$type][$name])) !== false)
		{
			unset(static::$migrations[$type][$name][$key]);
		}

		// make sure the migrations are in the correct order
		sort(static::$migrations[$type][$name]);

		// and save the update to the config file
		\Config::set('migrations.version.'.$type.'.'.$name, static::$migrations[$type][$name]);
		\Config::save(\Fuel::$env.DS.'migrations', 'migrations');
	}

	/**
	 * migrate down to the previous version
	 *
	 * @param   string  name of the package, module or app
	 * @param   string  type of migration (package, module or app)
	 * @param	mixed	version to start migrations from, or null to start at the beginning
	 * @param	mixed	version to end migrations by, or null to migrate to the end
	 *
	 * @return	array
	 */
	protected static function find_migrations($name, $type, $start = null, $end = null, $direction = 'up')
	{
		// Load all *_*.php files in the migrations path
		$method = '_find_'.$type;
		if ( ! $files = static::$method($name))
		{
			return array();
		}

		// get the currently installed migrations from the DB
		$current = \Arr::get(static::$migrations, $type.'.'.$name, array());

		// storage for the result
		$migrations = array();

		// normalize start and end values
		if ( ! is_null($start))
		{
			// if we have a prefix, use that
			($pos = strpos($start, '_')) === false or $start = ltrim(substr($start, 0, $pos), '0');
			is_numeric($start) and $start = (int) $start;
		}
		if ( ! is_null($end))
		{
			// if we have a prefix, use that
			($pos = strpos($end, '_')) === false or $end = ltrim(substr($end, 0, $pos), '0');
			is_numeric($end) and $end = (int) $end;
		}

		// filter the migrations out of bounds
		foreach ($files as $file)
		{
			// get the version for this migration and normalize it
			$migration = basename($file);
			($pos = strpos($migration, '_')) === false or $migration = ltrim(substr($migration, 0, $pos), '0');
			is_numeric($migration) and $migration = (int) $migration;

			// add the file to the migrations list if it's in between version bounds
			if ((is_null($start) or $migration > $start) and (is_null($end) or $migration <= $end))
			{
				// see if it is already installed
				if ( in_array(basename($file, '.php'), $current))
				{
					// already installed. store it only if we're going down
					$direction == 'down' and $migrations[$migration] = array('path' => $file);
				}
				else
				{
					// not installed yet. store it only if we're going up
					$direction == 'up' and $migrations[$migration] = array('path' => $file);
				}
			}
		}

		// We now prepare to actually DO the migrations
		// But first let's make sure that everything is the way it should be
		foreach ($migrations as $ver => $migration)
		{
			// get the migration filename from the path
			$migration['file'] = basename($migration['path']);

			// make sure the migration filename has a valid format
			if (preg_match('/^.*?_(.*).php$/', $migration['file'], $match))
			{
				// determine the classname for this migration
				$class_name = ucfirst(strtolower($match[1]));

				// load the file and determiine the classname
				include $migration['path'];
				$class = static::$prefix.$class_name;

				// make sure it exists in the migration file loaded
				if ( ! class_exists($class, false))
				{
					throw new FuelException(sprintf('Migration "%s" does not contain expected class "%s"', $migration['path'], $class));
				}

				// and that it contains an "up" and "down" method
				if ( ! is_callable(array($class, 'up')) or ! is_callable(array($class, 'down')))
				{
					throw new FuelException(sprintf('Migration class "%s" must include public methods "up" and "down"', $name));
				}

				$migrations[$ver]['class'] = $class;
			}
			else
			{
				throw new FuelException(sprintf('Invalid Migration filename "%s"', $migration['path']));
			}
		}

		// make sure the result is sorted properly with all version types
		uksort($migrations, 'strnatcasecmp');

		return $migrations;
	}

	/**
	 * finds migrations for the given app
	 *
	 * @param   string	name of the app (not used at the moment)
	 *
	 * @return  array
	 */
	protected static function _find_app($name = null)
	{
		return glob(APPPATH.\Config::get('migrations.folder').'*_*.php');
	}

	/**
	 * finds migrations for the given module (or all if name is not given)
	 *
	 * @param   string	name of the module
	 *
	 * @return  array
	 */
	protected static function _find_module($name = null)
	{
		$files = array();

		if ($name)
		{
			// find a module
			foreach (\Config::get('module_paths') as $m)
			{
				$files = glob($m .$name.'/'.\Config::get('migrations.folder').'*_*.php');
				if (count($files))
				{
					break;
				}
			}
		}
		else
		{
			// find all modules
			foreach (\Config::get('module_paths') as $m)
			{
				$files = array_merge($files, glob($m.'*/'.\Config::get('migrations.folder').'*_*.php'));
			}
		}

		return $files;
	}

	/**
	 * finds migrations for the given package (or all if name is not given)
	 *
	 * @param   string	name of the package
	 *
	 * @return  array
	 */
	protected static function _find_package($name = null)
	{
		$files = array();

		if ($name)
		{
			// find a package
			foreach (\Config::get('package_paths', array(PKGPATH)) as $p)
			{
				$files = glob($p .$name.'/'.\Config::get('migrations.folder').'*_*.php');
				if (count($files))
				{
					break;
				}
			}
		}
		else
		{
			// find all packages
			foreach (\Config::get('package_paths', array(PKGPATH)) as $p)
			{
				$files = array_merge($files, glob($p.'*/'.\Config::get('migrations.folder').'*_*.php'));
			}
		}

		return $files;
	}

	/**
	 * installs or upgrades the migration table to the current schema
	 *
	 * @return	void
	 *
	 * @deprecated	Remove upgrade check in 1.4
	 */
	protected static function table_version_check()
	{
		// set connection
		static::$connection === null or \DBUtil::set_connection(static::$connection);

		// if table does not exist
		if ( ! \DBUtil::table_exists(static::$table))
		{
			// create table
			\DBUtil::create_table(static::$table, static::$table_definition);
		}

		// check if a table upgrade is needed
		elseif ( ! \DBUtil::field_exists(static::$table, array('migration')))
		{
			// get the current migration status
			$current = \DB::select()->from(static::$table)->order_by('type', 'ASC')->order_by('name', 'ASC')->execute(static::$connection)->as_array();

			// drop the existing table, and recreate it in the new layout
			\DBUtil::drop_table(static::$table);
			\DBUtil::create_table(static::$table, static::$table_definition);

			// check if we had a current migration status
			if ( ! empty($current))
			{
				// do we need to migrate from a v1.0 migration environment?
				if (isset($current[0]['current']))
				{
					// convert the current result into a v1.1. migration environment structure
					$current = array(0 => array('name' => 'default', 'type' => 'app', 'version' => $current[0]['current']));
				}

				// build a new config structure
				$configs = array();

				// convert the v1.1 structure to the v1.2 structure
				foreach ($current as $migration)
				{
					// find the migrations for this entry
					$migrations = static::find_migrations($migration['name'], $migration['type'], null, $migration['version']);

					// array to keep track of the migrations already run
					$config = array();

					// add the individual migrations found
					foreach ($migrations as $file)
					{
						$file = pathinfo($file['path']);

						// add this migration to the table
						\DB::insert(static::$table)->set(array(
							'name' => $migration['name'],
							'type' => $migration['type'],
							'migration' => $file['filename'],
						))->execute(static::$connection);

						// and to the config
						$config[] = $file['filename'];
					}

					// create a config entry for this name and type if needed
					isset($configs[$migration['type']]) or $configs[$migration['type']] = array();
					$configs[$migration['type']][$migration['name']] = $config;
				}

				// write the updated migrations config back
				\Config::set('migrations.version', $configs);
				\Config::save(\Fuel::$env.DS.'migrations', 'migrations');
			}

			// delete any old migration config file that may exist
			file_exists(APPPATH.'config'.DS.'migrations.php') and unlink(APPPATH.'config'.DS.'migrations.php');
		}

		// set connection to default
		static::$connection === null or \DBUtil::set_connection(null);
	}
}
