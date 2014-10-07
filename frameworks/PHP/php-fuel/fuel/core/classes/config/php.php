<?php

namespace Fuel\Core;

/**
 * PHP Config file parser
 */
class Config_Php extends \Config_File
{
	protected $ext = '.php';

	/**
	 * Loads in the given file and parses it.
	 *
	 * @param   string  $file  File to load
	 * @return  array
	 */
	protected function load_file($file)
	{
		return \Fuel::load($file);
	}

	/**
	 * Returns the formatted config file contents.
	 *
	 * @param   array   $content  config array
	 * @return  string  formatted config file contents
	 */
	protected function export_format($contents)
	{
		$output = <<<CONF
<?php

CONF;
		$output .= 'return '.str_replace(array('  ', 'array (', '\''.APPPATH, '\''.DOCROOT, '\''.COREPATH, '\''.PKGPATH), array("\t", 'array(', 'APPPATH.\'', 'DOCROOT.\'', 'COREPATH.\'', 'PKGPATH.\''), var_export($contents, true)).";\n";
		return $output;
	}
}
