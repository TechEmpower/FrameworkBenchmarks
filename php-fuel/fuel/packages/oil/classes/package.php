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
 * Oil\Package Class
 *
 * @package		Fuel
 * @subpackage	Oil
 * @category	Core
 * @author		Phil Sturgeon
 */
class Package
{
	protected static $protected = array('oil');

	protected static $git = 'git';

	public static function install($package = null)
	{
		// Make sure something is set
		if ($package === null)
		{
			static::help();
			return;
		}

		$config = \Config::load('package');

		$version = \Cli::option('version', 'master');

		// Check to see if this package is already installed
		if (is_dir(PKGPATH . $package))
		{
			throw new Exception('Package "' . $package . '" is already installed.');
			return;
		}

		foreach ($config['sources'] as $source)
		{
			$packages = array('fuel-'.$package, $package);

			foreach ($packages as $package)
			{
				$zip_url = 'http://' . rtrim($source, '/').'/'.$package.'/zipball/'.$version;

				if ($fp = @fopen($zip_url, 'r'))
				{
					// We don't actually need this, just checking the file is there
					fclose($fp);

					// Now, lets get this package

					// If a direct download is requested, or git is unavailable, download it!
					if (\Cli::option('direct') OR static::_use_git() === false)
					{
						static::_download_package_zip($zip_url, $package, $version);
						exit;
					}

					// Otherwise, get your clone on baby!
					else
					{
						static::_clone_package_repo($source, $package, $version);
						exit;
					}
				}
			}
		}

		throw new Exception('Could not find package "' . $package . '".');
	}


	public static function uninstall($package)
	{
		$package_folder = PKGPATH . $package;

		// Check to see if this package is already installed
		if (in_array($package, static::$protected))
		{
			throw new Exception('Package "' . $package . '" cannot be uninstalled.');
			return false;
		}

		// Check to see if this package is already installed
		if ( ! is_dir($package_folder))
		{
			throw new Exception('Package "' . $package . '" is not installed.');
			return false;
		}

		\Cli::write('Package "' . $package . '" was uninstalled.', 'yellow');

		\File::delete_dir($package_folder);
	}

	public static function help()
	{
		$output = <<<HELP

Usage:
  php oil [p|package] <packagename>

Description:
  Packages containing extra functionality can be downloaded (or git cloned) simply with
  the following commands.

Runtime options:
  --direct       # Download direct from ZIP even if Git is installed

Examples:
  php oil package install <packagename>
  php oil package uninstall <packagename>

Documentation:
  http://fuelphp.com/docs/packages/oil/package.html
HELP;
		\Cli::write($output);

	}


	private static function _use_git()
	{
		exec('which git', $output);

		// If this is a valid path to git, use it instead of just "git"
		if (file_exists($line = trim(current($output))))
		{
			static::$git = $line;
		}

		unset($output);

		// Double check git is installed (windows will fail step 1)
		exec(static::$git . ' --version', $output);

		preg_match('#^(git version)#', current($output), $matches);

		// If we have a match, use Git!
		return ! empty($matches[0]);
	}

	private static function _download_package_zip($zip_url, $package, $version)
	{
		\Cli::write('Downloading package: ' . $zip_url);

		// Make the folder so we can extract the ZIP to it
		mkdir($tmp_folder = APPPATH . 'tmp/' . $package . '-' . time());

		$zip_file = $tmp_folder . '.zip';
		@copy($zip_url, $zip_file);

		if (file_exists($zip_file))
		{
			$unzip = new \Unzip;
			$files = $unzip->extract($zip_file, $tmp_folder);

			// Grab the first folder out of it (we dont know what it's called)
			list($tmp_package_folder) = glob($tmp_folder.'/*', GLOB_ONLYDIR);

			$package_folder = PKGPATH . $package;

			// Move that folder into the packages folder
			rename($tmp_package_folder, $package_folder);

			unlink($zip_file);
			rmdir($tmp_folder);

			foreach ($files as $file)
			{
				$path = str_replace($tmp_package_folder, $package_folder, $file);
				chmod($path, octdec(755));
				\Cli::write("\t" . $path);
			}
		}
		else
		{
			\Cli::write('Package could not be found', 'red');
		}
	}

	public static function _clone_package_repo($source, $package, $version)
	{
		$repo_url = 'git://' . rtrim($source, '/').'/'.$package . '.git';

		\Cli::write('Downloading package: ' . $repo_url);

		$package_folder = PKGPATH . $package;

		// Clone to the package path
		passthru(static::$git . ' clone ' . $repo_url . ' ' . $package_folder);

		passthru(static::$git .' add ' . $package_folder . '/');

		\Cli::write('');
	}
}

/* End of file package.php */
