<?php
/**
 * Fuel is a fast, lightweight, community driven PHP5 framework.
 *
 * @package    Fuel
 * @version    1.5
 * @author     Fuel Development Team
 * @license    MIT License
 * @copyright  2010 - 2013 Fuel Development Team
 * @link       http://fuelphp.com
 */

namespace Oil;

/**
 * Oil\Refine Class
 *
 * @package		Fuel
 * @subpackage	Oil
 * @category	Core
 */
class Refine
{
	public static function run($task, $args)
	{
		$task = strtolower($task);

		// Make sure something is set
		if (empty($task) or $task === 'help')
		{
			static::help();
			return;
		}

		$module = false;
		list($module, $task) = array_pad(explode('::', $task), 2, null);

		if ($task === null)
		{
			$task = $module;
			$module = false;
		}

		if ($module)
		{
			try
			{
				\Module::load($module);
				$path = \Module::exists($module);
				\Finder::instance()->add_path($path);
			}
			catch (\FuelException $e)
			{
				throw new Exception(sprintf('Module "%s" does not exist.', $module));
			}
		}

		// Just call and run() or did they have a specific method in mind?
		list($task, $method) = array_pad(explode(':', $task), 2, 'run');

		// Find the task
		if ( ! $file = \Finder::search('tasks', $task))
		{
			$files = \Finder::instance()->list_files('tasks');
			$possibilities = array();
			foreach($files as $file)
			{
				$possible_task = pathinfo($file, \PATHINFO_FILENAME);
				$difference = levenshtein($possible_task, $task);
				$possibilities[$difference] = $possible_task;
			}

			ksort($possibilities);

			if ($possibilities and current($possibilities) <= 5)
			{
				throw new Exception(sprintf('Task "%s" does not exist. Did you mean "%s"?', $task, current($possibilities)));
			}
			else
			{
				throw new Exception(sprintf('Task "%s" does not exist.', $task));
			}

			return;
		}

		require_once $file;

		$task = '\\Fuel\\Tasks\\'.ucfirst($task);

		$new_task = new $task;

		// The help option hs been called, so call help instead
		if (\Cli::option('help') && is_callable(array($new_task, 'help')))
		{
			$method = 'help';
		}

		if ($return = call_user_func_array(array($new_task, $method), $args))
		{
			\Cli::write($return);
		}
	}

	public static function help()
	{
	    // Build a list of possible tasks for the help output
		$tasks = self::_discover_tasks();
		if (count($tasks) > 0)
		{
			$output_available_tasks = "";

			foreach ($tasks as $task => $options)
			{
				foreach ($options as $option)
				{
				    $option = ($option == "run") ? "" : ":$option";
					$output_available_tasks .= "    php oil refine $task$option\n";
				}
			}
		}

		else
		{
			$output_available_tasks = "    (none found)";
		}

		$output = <<<HELP

Usage:
    php oil [r|refine] <taskname>

Description:
    Tasks are classes that can be run through the the command line or set up as a cron job.

Available tasks:
$output_available_tasks
Documentation:
    http://docs.fuelphp.com/packages/oil/refine.html
HELP;
		\Cli::write($output);

	}

	/**
	 * Find all of the task classes in the system and use reflection to discover the
	 * commands we can call.
	 *
	 * @return array $taskname => array($taskmethods)
	 **/
	protected static function _discover_tasks()
	{
		$result = array();
		$files = \Finder::instance()->list_files('tasks');

		if (count($files) > 0)
		{
			foreach ($files as $file)
			{
				$task_name = str_replace('.php', '', basename($file));
				$class_name = '\\Fuel\\Tasks\\'.$task_name;

				require $file;

				$reflect = new \ReflectionClass($class_name);

				// Ensure we only pull out the public methods
				$methods = $reflect->getMethods(\ReflectionMethod::IS_PUBLIC);

				$result[$task_name] = array();

				if (count($methods) > 0)
				{
					foreach ($methods as $method)
					{
						strpos($method->name, '_') !== 0 and $result[$task_name][] = $method->name;
					}
				}
			}
		}

		return $result;
	}
}
