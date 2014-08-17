<?php

namespace Fuel\Core;

/**
 * Yaml Lang file parser
 */
class Lang_Yml extends \Lang_File
{
	protected $ext = '.yml';

	/**
	 * Loads in the given file and parses it.
	 *
	 * @param   string  $file  File to load
	 * @return  array
	 */
	protected function load_file($file)
	{
		$contents = $this->parse_vars(file_get_contents($file));
		return \Format::forge($contents, 'yaml')->to_array();
	}

	/**
	 * Returns the formatted language file contents.
	 *
	 * @param   array   $content  config array
	 * @return  string  formatted config file contents
	 */
	protected function export_format($contents)
	{
		if ( ! function_exists('spyc_load'))
		{
			import('spyc/spyc', 'vendor');
		}

		$this->prep_vars($contents);
		return \Spyc::YAMLDump($contents);
	}
}
