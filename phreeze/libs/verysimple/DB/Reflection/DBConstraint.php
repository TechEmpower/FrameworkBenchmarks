<?php
/** @package    verysimple::DB::Reflection */

/**
 * DBSet is an object representation of foreign key constraint
 *
 * @package    verysimple::DB::Reflection
 * @author Jason Hinkle
 * @copyright  1997-2007 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version 1.0
 */
 class DBConstraint
{
	public $Table;
	public $Name;
	public $KeyColumn;
	public $ReferenceTable;
	public $ReferenceTableName;
	public $ReferenceKeyColumn;
	
	public $NameNoPrefix;
	public $KeyColumnNoPrefix;
	public $ReferenceKeyColumnNoPrefix;
	
	public $GetterName;
	
	/**
	 * Instantiate new DBConstraint
	 *
	 * @access public
	 * @param DBTable $table that is the dependent/child table
	 * @param Array $row array that is result from parsing show create table	
	 */	
	function __construct($table, $row)
	{
		$this->Table =& $table;
		
		$this->Name = $row[0];
		$this->KeyColumn = $row[1];
		$this->ReferenceTableName = $row[2];
		$this->ReferenceKeyColumn = $row[3];

		$this->ReferenceTable = $this->Table->Schema->Tables[$this->ReferenceTableName];
		// print "<p><b>" . $this->Table->Name . " constraint references " . $reftable->Name . "</b></p>";

		$this->NameNoPrefix = $this->Table->RemovePrefix($this->Name);
		$this->KeyColumnNoPrefix = $this->Table->RemovePrefix($this->KeyColumn);
		$this->ReferenceKeyColumnNoPrefix = $this->ReferenceTable->RemovePrefix($this->ReferenceKeyColumn);

		// intelligently decide what a good name for this constraint might be
		$tmp1 = str_replace("__","_",str_replace($this->ReferenceTableName,"",str_replace("_id","", $this->KeyColumnNoPrefix)) . "_");
		$tmp2 = $this->ReferenceTableName;
		$this->GetterName = ($tmp1 == "_") ? $tmp2 : ($tmp1 . $tmp2);
	}
}

?>