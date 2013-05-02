<?php

namespace Fuel\Core;

/**
 * JSON Config file parser
 */
class Config_Json extends \Config_File
{
	protected $ext = '.json';

	/**
	 * Loads in the given file and parses it.
	 *
	 * @param   string  $file  File to load
	 * @return  array
	 */
	protected function load_file($file)
	{
		$contents = $this->parse_vars(file_get_contents($file));
		return json_decode($contents, true);
	}

	/**
	 * Returns the formatted config file contents.
	 *
	 * @param   array   $content  config array
	 * @return  string  formatted config file contents
	 */
	protected function export_format($contents)
	{
		$this->prep_vars($contents);
		return \Format::forge()->to_json($contents, true);
	}
}
