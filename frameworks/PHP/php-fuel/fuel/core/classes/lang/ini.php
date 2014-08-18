<?php

namespace Fuel\Core;

/**
 * INI Lang file parser
 */
class Lang_Ini extends \Lang_File
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
	 * Returns the formatted language file contents.
	 *
	 * @param   array   $content  language array
	 * @return  string  formatted language file contents
	 */
	protected function export_format($contents)
	{
		throw new \LangException('Saving lang to ini is not supported at this time');
	}
}
