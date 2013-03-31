<?php
/** @package    verysimple::IO */

/** import supporting libraries */
require_once("FileHelper.php");

/**
 * Provided object oriented access to a file system directory
 *
 * @package    verysimple::IO
 * @author Jason Hinkle
 * @copyright  1997-2007 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version 1.0
 */
class FolderHelper
{
	private $Path;

	/**
	 * Constructor
	 *
	 * @access public
	 * @param string $path uri to directory to manipulate
	 */	
	function __construct($path)
	{
		$this->Path = $path;
	}
	
	/**
	 * Returns an array of FileHelper objects
	 *
	 * @access public
	 * @param string $pattern (not yet implemented)
	 * @return array
	 */	
	public function GetFiles($pattern = "")
	{
		$files = Array();
		$dh = opendir($this->Path);

		while ($fname = readdir($dh)) 
		{
			if (is_file($this->Path.$fname))
			{
				if ($pattern == "" || preg_match($pattern,$fname) > 0)
				{
					$files[] = new FileHelper($this->Path.$fname);
				}
			}
		}
		
		closedir($dh);
	    
		return $files;
	}

}
?>