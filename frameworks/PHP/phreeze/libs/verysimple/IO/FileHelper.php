<?php
/** @package    verysimple::IO */

/**
 * Provides helper functions for dealing with a file
 *
 * @package    verysimple::IO
 * @author Jason Hinkle
 * @copyright  1997-2007 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version 1.0
 */
 class FileHelper
{
	public $Name;
	public $Path;
	public $FolderPath;
	public $Extention;
	public $Prefix;
	public $MiddleBit;
	
	/**
	 * Creates a new instance of a FileHelper object
	 *
	 * @access public
	 * @param $path The full path to the file
	 */
	function __construct($path)
	{
		//TODO: user build-in php functions to extract these properties
		
		$this->Path = str_replace("\\","/", $path); // normalize any directory paths
		
		$this->Name = substr($this->Path, strrpos($this->Path,"/")+1);
		$this->Extention = substr($this->Path, strrpos($this->Path,".")+1);
		$this->Prefix = substr($this->Name, 0, strpos($this->Name,"."));
		$this->MiddleBit = substr($this->Name, strpos($this->Name,".")+1, strrpos($this->Name,".")-strpos($this->Name,".")-1);
		$this->FolderPath = substr($this->Path, 0, strrpos($this->Path,"/")+1);
	}
	
}
?>