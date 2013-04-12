<?php

namespace Fuel\Core;

/**
 * A base Config File class for File based configs.
 */
abstract class Config_File implements Config_Interface
{
	protected $file;

	protected $vars = array();

	/**
	 * Sets up the file to be parsed and variables
	 *
	 * @param   string  $file  Config file name
	 * @param   array   $vars  Variables to parse in the file
	 * @return  void
	 */
	public function __construct($file = null, $vars = array())
	{
		$this->file = $file;

		$this->vars = array(
			'APPPATH' => APPPATH,
			'COREPATH' => COREPATH,
			'PKGPATH' => PKGPATH,
			'DOCROOT' => DOCROOT,
		) + $vars;
	}

	/**
	 * Loads the config file(s).
	 *
	 * @param   bool  $overwrite  Whether to overwrite existing values
	 * @return  array  the config array
	 */
	public function load($overwrite = false, $cache = true)
	{
		$paths = $this->find_file($cache);
		$config = array();

		foreach ($paths as $path)
		{
			$config = $overwrite ?
				array_merge($config, $this->load_file($path)) :
				\Arr::merge($config, $this->load_file($path));
		}

		return $config;
	}

	/**
	 * Gets the default group name.
	 *
	 * @return  string
	 */
	public function group()
	{
		return $this->file;
	}

	/**
	 * Parses a string using all of the previously set variables.  Allows you to
	 * use something like %APPPATH% in non-PHP files.
	 *
	 * @param   string  $string  String to parse
	 * @return  string
	 */
	protected function parse_vars($string)
	{
		foreach ($this->vars as $var => $val)
		{
			$string = str_replace("%$var%", $val, $string);
		}

		return $string;
	}

	/**
	 * Replaces FuelPHP's path constants to their string counterparts.
	 *
	 * @param   array  $array  array to be prepped
	 * @return  array  prepped array
	 */
	protected function prep_vars(&$array)
	{
		static $replacements = false;

		if ($replacements === false)
		{
			foreach ($this->vars as $i => $v)
			{
				$replacements['#^('.preg_quote($v).'){1}(.*)?#'] = "%".$i."%$2";
			}
		}

		foreach ($array as $i => $value)
		{
			if (is_string($value))
			{
				$array[$i] = preg_replace(array_keys($replacements), array_values($replacements), $value);
			}
			elseif(is_array($value))
			{
				$this->prep_vars($array[$i]);
			}
		}
	}

	/**
	 * Finds the given config files
	 *
	 * @param   bool  $multiple  Whether to load multiple files or not
	 * @return  array
	 */
	protected function find_file($cache = true)
	{
		if (($this->file[0] === '/' or (isset($this->file[1]) and $this->file[1] === ':')) and is_file($this->file))
		{
			$paths = array($this->file);
		}
		else
		{
			$paths = array_merge(
				\Finder::search('config/'.\Fuel::$env, $this->file, $this->ext, true, $cache),
				\Finder::search('config', $this->file, $this->ext, true, $cache)
			);
		}

		if (empty($paths))
		{
			throw new \ConfigException(sprintf('File "%s" does not exist.', $this->file));
		}

		return array_reverse($paths);
	}

	/**
	 * Formats the output and saved it to disc.
	 *
	 * @param   $contents  $contents    config array to save
	 * @return  bool       \File::update result
	 */
	public function save($contents)
	{
		// get the formatted output
		$output = $this->export_format($contents);

		if ( ! $output)
		{
			return false;
		}

		if ( ! $path = \Finder::search('config', $this->file, $this->ext))
		{
			if ($pos = strripos($this->file, '::'))
			{
				// get the namespace path
				if ($path = \Autoloader::namespace_path('\\'.ucfirst(substr($this->file, 0, $pos))))
				{
					// strip the namespace from the filename
					$this->file = substr($this->file, $pos+2);

					// strip the classes directory as we need the module root
					$path = substr($path,0, -8).'config'.DS.$this->file;
				}
				else
				{
					// invalid namespace requested
					return false;
				}
			}
		}

		// absolute path requested?
		if ($this->file[0] === '/' or (isset($this->file[1]) and $this->file[1] === ':'))
		{
			$path = $this->file;
		}

		// make sure we have a fallback
		$path or $path = APPPATH.'config'.DS.$this->file.$this->ext;

		$path = pathinfo($path);
		if ( ! is_dir($path['dirname']))
		{
			mkdir($path['dirname'], 0777, true);
		}

		return \File::update($path['dirname'], $path['basename'], $output);
	}

	/**
	 * Must be implemented by child class. Gets called for each file to load.
	 */
	abstract protected function load_file($file);

	/**
	 * Must be impletmented by child class. Gets called when saving a config file.
	 *
	 * @param   array   $contents  config array to save
	 * @return  string  formatted output
	 */
	abstract protected function export_format($contents);
}
