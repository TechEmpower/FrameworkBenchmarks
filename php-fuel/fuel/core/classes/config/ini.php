<?php

namespace Fuel\Core;

/**
 * INI Config file parser
 */
class Config_Ini extends \Config_File
{
	protected $ext = '.ini';

	/**
	 * Loads in the given file and parses it.
	 *
	 * @param   string  $file  File to load
	 * @return  array
	 */
	protected function load_file($file)
	{
		$contents = $this->parse_vars(file_get_contents($file));
		return parse_ini_string($contents, true);
	}

	/**
	 * Returns the formatted config file contents.
	 *
	 * @param   array   $content  config array
	 * @return  string  formatted config file contents
	 */
	protected function export_format($contents)
	{
		throw new \ConfigException('Saving config to ini is not supported at this time');
	}
}
