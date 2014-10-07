<?php

namespace Fuel\Core;

/**
 * A base Lang File class for File based configs.
 */
abstract class Lang_File implements Lang_Interface
{
	protected $file;

	protected $languages = array();

	protected $vars = array();

	/**
	 * Sets up the file to be parsed and variables
	 *
	 * @param   string  $file       Lang file name
	 * @param   array   $languages  Languages to scan for the lang file
	 * @param   array   $vars       Variables to parse in the file
	 * @return  void
	 */
	public function __construct($file = null, $languages = array(), $vars = array())
	{
		$this->file = $file;

		$this->languages = $languages;

		$this->vars = array(
			'APPPATH' => APPPATH,
			'COREPATH' => COREPATH,
			'PKGPATH' => PKGPATH,
			'DOCROOT' => DOCROOT,
		) + $vars;
	}

	/**
	 * Loads the language file(s).
	 *
	 * @param   bool  $overwrite  Whether to overwrite existing values
	 * @return  array  the language array
	 */
	public function load($overwrite = false)
	{
		$paths = $this->find_file();
		$lang = array();

		foreach ($paths as $path)
		{
			$lang = $overwrite ?
				array_merge($lang, $this->load_file($path)) :
				\Arr::merge($lang, $this->load_file($path));
		}

		return $lang;
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
	 * Finds the given language files
	 *
	 * @param   bool  $multiple  Whether to load multiple files or not
	 * @return  array
	 */
	protected function find_file()
	{
		$paths = array();
		foreach ($this->languages as $lang)
		{
			$paths = array_merge($paths, \Finder::search('lang'.DS.$lang, $this->file, $this->ext, true));
		}

		if (count($paths) > 0)
		{
			return array_reverse($paths);
		}

		throw new \LangException(sprintf('File "%s" does not exist.', $this->file));
	}

	/**
	 * Formats the output and saved it to disc.
	 *
	 * @param   string     $identifier  filename
	 * @param   $contents  $contents    language array to save
	 * @return  bool       \File::update result
	 */
	public function save($identifier, $contents)
	{
		// get the formatted output
		$output = $this->export_format($contents);

		if ( ! $output)
		{
			return false;
		}

		if ( ! $path = \Finder::search('lang', $identifier))
		{
			if ($pos = strripos($identifier, '::'))
			{
				// get the namespace path
				if ($path = \Autoloader::namespace_path('\\'.ucfirst(substr($identifier, 0, $pos))))
				{
					// strip the namespace from the filename
					$identifier = substr($identifier, $pos+2);

					// strip the classes directory as we need the module root
					$path = substr($path,0, -8).'lang'.DS.$identifier;
				}
				else
				{
					// invalid namespace requested
					return false;
				}
			}
		}

		// absolute path requested?
		if ($identifier[0] === '/' or (isset($identifier[1]) and $identifier[1] === ':'))
		{
			$path = $identifier;
		}

		// make sure we have a fallback
		$path or $path = APPPATH.'lang'.DS.$identifier;

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
	 * Must be impletmented by child class. Gets called when saving a language file.
	 *
	 * @param   array   $contents  language array to save
	 * @return  string  formatted output
	 */
	abstract protected function export_format($contents);
}
