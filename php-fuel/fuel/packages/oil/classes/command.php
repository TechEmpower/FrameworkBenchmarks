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
 * Oil\Cli Class
 *
 * @package		Fuel
 * @subpackage	Oil
 * @category	Core
 */
class Command
{
	public static function init($args)
	{
		\Config::load('oil', true);

		// Remove flag options from the main argument list
		$args = self::_clear_args($args);

		try
		{
			if ( ! isset($args[1]))
			{
				if (\Cli::option('v', \Cli::option('version')))
				{
					\Cli::write('Fuel: '.\Fuel::VERSION.' running in "'.\Fuel::$env.'" mode');
					return;
				}

				static::help();
				return;
			}

			switch ($args[1])
			{
				case 'g':
				case 'generate':

					$action = isset($args[2]) ? $args[2]: 'help';

					$subfolder = 'orm';
					if (is_int(strpos($action, '/')))
					{
						list($action, $subfolder)=explode('/', $action);
					}

					switch ($action)
					{
						case 'config':
						case 'controller':
						case 'model':
						case 'migration':
						case 'task':
							call_user_func('Oil\Generate::'.$action, array_slice($args, 3));
						break;

						case 'views':
							call_user_func('Oil\Generate::views', array_slice($args, 3), $subfolder);
						break;

						case 'admin':
							call_user_func('Oil\Generate_Admin::forge', array_slice($args, 3), $subfolder);
						break;

						case 'scaffold':
							call_user_func('Oil\Generate_Scaffold::forge', array_slice($args, 3), $subfolder);
						break;

						default:
							Generate::help();
					}

				break;

				case 'c':
				case 'console':
					new Console;

				case 'p':
				case 'package':

					$action = isset($args[2]) ? $args[2]: 'help';

					switch ($action)
					{
						case 'install':
						case 'uninstall':
							call_user_func_array('Oil\Package::'.$action, array_slice($args, 3));
						break;

						default:
							Package::help();
					}

				break;

				case 'r':
				case 'refine':

					// Developers of third-party tasks may not be displaying PHP errors. Report any error and quit
					set_error_handler(function($errno, $errstr, $errfile, $errline) {
						if (!error_reporting()) return; // If the error was supressed with an @ then we ignore it!

						\Cli::error("Error: {$errstr} in $errfile on $errline");
						\Cli::beep();
						exit(1);
					});

					$task = isset($args[2]) ? $args[2] : null;

					call_user_func('Oil\Refine::run', $task, array_slice($args, 3));
				break;

				case 'cell':
				case 'cells':

					$action = isset($args[2]) ? $args[2]: 'help';

					switch ($action)
					{
						case 'list':
							call_user_func('Oil\Cell::all');
						break;

						case 'search':
						case 'install':
						case 'upgrade':
						case 'uninstall':
							call_user_func_array('Oil\Cell::'.$action, array_slice($args, 3));
						break;

						case 'info':
						case 'details':
							call_user_func_array('Oil\Cell::info', array_slice($args, 3));
						break;

						default:
							Cell::help();
					}

				break;

				case 't':
				case 'test':

					// Suppressing this because if the file does not exist... well thats a bad thing and we can't really check
					// I know that supressing errors is bad, but if you're going to complain: shut up. - Phil
					$phpunit_autoload_path = \Config::get('oil.phpunit.autoload_path', 'PHPUnit/Autoload.php' );
					@include_once($phpunit_autoload_path);

					// Attempt to load PHUnit.  If it fails, we are done.
					if ( ! class_exists('PHPUnit_Framework_TestCase'))
					{
						throw new Exception('PHPUnit does not appear to be installed.'.PHP_EOL.PHP_EOL."\tPlease visit http://phpunit.de and install.");
					}

					// Check for a custom phpunit config, but default to the one from core
					if (file_exists(APPPATH.'phpunit.xml'))
					{
						$phpunit_config = APPPATH.'phpunit.xml';
					}
					else
					{
						$phpunit_config = COREPATH.'phpunit.xml';
					}

					// CD to the root of Fuel and call up phpunit with the path to our config
					$phpunit_command = \Config::get('oil.phpunit.binary_path', 'phpunit');
					$command = 'cd '.DOCROOT.'; '.$phpunit_command.' -c "'.$phpunit_config.'"';

					// Respect the group options
					\Cli::option('group') and $command .= ' --group '.\Cli::option('group');
					\Cli::option('exclude-group') and $command .= ' --exclude-group '.\Cli::option('exclude-group');

					// Respect the coverage-html option
					\Cli::option('coverage-html') and $command .= ' --coverage-html '.\Cli::option('coverage-html');
					\Cli::option('coverage-clover') and $command .= ' --coverage-clover '.\Cli::option('coverage-clover');
					\Cli::option('coverage-text') and $command .= ' --coverage-text='.\Cli::option('coverage-text');
					\Cli::option('coverage-php') and $command .= ' --coverage-php '.\Cli::option('coverage-php');

					\Cli::write('Tests Running...This may take a few moments.', 'green');

					$return_code = 0;
					foreach(explode(';', $command) as $c)
					{
						passthru($c, $return_code_task);
						// Return failure if any subtask fails
						$return_code |= $return_code_task;
					}
					exit($return_code);
				break;

				default:

					static::help();
			}
		}

		catch (Exception $e)
		{
			\Cli::error('Error: '.$e->getMessage());
			\Cli::beep();

			\Cli::option('speak') and `say --voice="Trinoids" "{$e->getMessage()}"`;
			exit(1);
		}
	}

	public static function help()
	{
		echo <<<HELP

Usage:
  php oil [cell|console|generate|package|refine|help|test]

Runtime options:
  -f, [--force]    # Overwrite files that already exist
  -s, [--skip]     # Skip files that already exist
  -q, [--quiet]    # Supress status output
  -t, [--speak]    # Speak errors in a robot voice

Description:
  The 'oil' command can be used in several ways to facilitate quick development, help with
  testing your application and for running Tasks.

Environment:
  If you want to specify a specific environment oil has to run in, overload the environment
  variable on the commandline: FUEL_ENV=staging php oil <commands>

Documentation:
  http://docs.fuelphp.com/packages/oil/intro.html

HELP;

	}

	private static function _clear_args($actions = array())
	{
		foreach ($actions as $key => $action)
		{
			if (substr($action, 0, 1) === '-')
			{
				unset($actions[$key]);
			}
		}

		return $actions;
	}
}

/* End of file oil/classes/command.php */
