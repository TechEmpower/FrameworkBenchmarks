<?php
/**
 * Dir
 *
 * Provides basic directory functions such as recursion and creation.
 *
 * @package		MicroMVC
 * @author		David Pennington
 * @copyright	(c) 2011 MicroMVC Framework
 * @license		http://micromvc.com/license
 ********************************** 80 Columns *********************************
 */
namespace Micro;

class Directory
{

	/**
	 * Create a recursive directory iterator object
	 *
	 * @param string $dir the directory to load
	 * @param boolean $recursive to include subfolders
	 * @return object
	 */
	static function load($dir, $recursive = FALSE)
	{
		$i = new \RecursiveDirectoryIterator($dir);

		if(! $recursive) return $i;

		return new \RecursiveIteratorIterator($i, \RecursiveIteratorIterator::SELF_FIRST);
	}


	/**
	 * Create an array of all (or just one of) file/folders/link objects in a directory.
	 *
	 * @param string $dir the directory to load
	 * @param boolean $recursive to include subfolders
	 * @param string $only set to one of "file", "dir", or "link" to filter results
	 * @return array
	 */
	static function contents($dir, $recursive = FALSE, $only = FALSE)
	{
		$dir = self::load($dir, $recursive);

		if(!$only) return $dir;

		$only = "is$only";

		$results = array();

		foreach($dir as $file)
		{
			if($file->$only()) $results[] = $file;
		}

		return $results;
	}


	/**
	 * Make sure that a directory exists and is writable by the current PHP process.
	 *
	 * @param string $dir the directory to load
	 * @param string $chmod
	 * @return boolean
	 */
	static function usable($dir, $chmod = '0744')
	{
		// If it doesn't exist, and can't be made
		if(! is_dir($dir) AND ! mkdir($dir, $chmod, TRUE)) return FALSE;

		// If it isn't writable, and can't be made writable
		if(! is_writable($dir) AND !chmod($dir, $chmod)) return FALSE;

		return TRUE;
	}

}

// END
