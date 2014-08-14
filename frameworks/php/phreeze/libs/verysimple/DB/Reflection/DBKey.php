<?php
/** @package    verysimple::DB::Reflection */

/**
 * DBSet is an object representation of foreign key
 *
 * @package    verysimple::DB::Reflection
 * @author Jason Hinkle
 * @copyright  1997-2007 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version 1.0
 */
class DBKey
{
	public $Table;
	public $Name;
	public $NameNoPrefix;
	public $GetterName;
	public $KeyColumn;
	public $KeyComment;

	/**
	 * Instantiate new DBSet
	 *
	 * @access public
	 * @param DBTable $table that is the dependent/child table
	 * @param string $keyname	
	 * @param string $columnname	
	 */	
	function __construct($table, $keyname, $columnname)
	{
		$this->Table =& $table;
		$this->Name = $keyname;
		$this->KeyColumn = str_replace("`","", $columnname);
		$this->KeyComment = $this->Table->Columns[$this->KeyColumn]->Comment;

		$this->NameNoPrefix = $this->Table->RemovePrefix($this->Name);
		$this->GetterName = $this->NameNoPrefix;
	}
}

?>