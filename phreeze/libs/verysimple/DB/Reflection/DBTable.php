<?php
/** @package    verysimple::DB::Reflection */

/** import supporting libraries */
require_once("DBColumn.php");
require_once("DBConstraint.php");
require_once("DBSet.php");
require_once("DBKey.php");

/**
 * DBTable is an object representation of a MySQL Table
 *
 * @package    verysimple::DB::Reflection
 * @author Jason Hinkle
 * @copyright  1997-2007 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version 1.0
 */
class DBTable
{
	public $Schema;
	public $Name;
	public $Engine;
	public $Comment;
	public $DefaultCharacterSet;
	public $Columns;
	public $ColumnPrefix;
	public $PrimaryKeys;
	public $ForeignKeys;
	public $Constraints;
	public $Sets;
	public $IsView = false;

	/**
	 * Instantiate new DBTable
	 *
	 * @access public
	 * @param DBSchema $schema
	 * @return Array $row array that is result from "show tables" statement
	 */
	function __construct($schema, $row)
	{
		$this->Schema = $schema;
		$this->Name = $row["Tables_in_" . $this->Schema->Name];
		$this->Columns = Array();
		$this->PrimaryKeys = Array();
		$this->ForeignKeys = Array();
		$this->Constraints = Array();
		$this->Sets = Array();

		$this->LoadColumns();
		$this->DiscoverColumnPrefix();


	}

	/**
	 * Returns the number of columns involved in the primary key
	 * @return number
	 */
	function NumberOfPrimaryKeyColumns()
	{
		return count($this->PrimaryKeys);
	}

	/**
	 * Returns name of the primary key
	 * TODO: If there are multiple keys, this is no accurate.  Only returns the first key found
	 *
	 * @access public
	 * @param bool $remove_prefix
	 * @return string
	 */
	function GetPrimaryKeyName($remove_prefix = true)
	{
		foreach ($this->PrimaryKeys as $key)
		{
			return ($remove_prefix) ? $this->RemovePrefix($key->KeyColumn) : $key->KeyColumn;
		}
		
		// views don't technically have a primary key but we will return the first column if anybody asks
		if ($this->IsView) return $this->GetColumnNameByIndex(0,$remove_prefix);
	}
	
	/**
	 * Returns the name of the column at the given index
	 *
	 * @access public
	 * @param int $index (zero based)
	 * @param bool $remove_prefix
	 * @return string
	 */
	function GetColumnNameByIndex($index, $remove_prefix = true)
	{
		$count = 0;
		foreach ($this->Columns as $column)
		{
			if ($count == $index) return ($remove_prefix) ? $column->NameWithoutPrefix : $column->Name;
		}
		
		throw new Exception('Index out of bounds');
	}

	/**
	 * Returns true if the primary key for this table is an auto_increment
	 * TODO: Only checks the first key if there are multiple primary keys
	 *
	 * @access public
	 * @return bool
	 */
	function PrimaryKeyIsAutoIncrement()
	{
		$pk = $this->GetPrimaryKeyName(false);
		return $pk && $this->Columns[$pk]->Extra == "auto_increment";
	}

	/**
	 * Returns name of the first varchar field which could be used as a "label"
	 *
	 * @access public
	 * @return string
	 */
	function GetDescriptorName($remove_prefix = true)
	{
		foreach ($this->Columns as $column)
		{
			if ($column->Type == "varchar")
			{
				return ($remove_prefix) ? $this->RemovePrefix($column->Name) : $column->Name;
			}
		}

		// give up because there are no varchars in this table
		return $this->GetPrimaryKeyName($remove_prefix);
	}

	/**
	 * Inspects all columns to see if there is a common prefix in the format: XXX_
	 *
	 * @access private
	 */
	private function DiscoverColumnPrefix()
	{
		$prev_prefix = "";
		$has_prefix = true;
		foreach ($this->Columns as $column)
		{
			$curr_prefix = substr($column->Name, 0, strpos($column->Name,"_")+1);

			if ($prev_prefix == "")
			{
				// first time through the loop
				$prev_prefix = $curr_prefix ? $curr_prefix : "#NONE#";
			}
			elseif ($prev_prefix != $curr_prefix)
			{
				$has_prefix = false;
			}
		}

		if ($has_prefix)
		{
			// set the table column prefix property
			$this->ColumnPrefix = $curr_prefix;

			// update the columns to reflect the prefix as well
			foreach ($this->Columns as $column)
			{
				$column->NameWithoutPrefix = substr($column->Name,strlen($curr_prefix));
			}
		}
	}

	/**Given a column name, removes the prefix
	 *
	 * @access private
	 */
	public function RemovePrefix($name)
	{
		// print "remove prefix $name: " . $this->ColumnPrefix . "<br>";
		return substr($name,strlen($this->ColumnPrefix));
	}


	/**
	 * Inspects the current table and loads all Columns
	 *
	 * @access private
	 */
	private function LoadColumns()
	{
		// get the colums
		$sql = "describe `".$this->Name."`";

		$rs = $this->Schema->Server->Connection->Select($sql);

		while ($row = $this->Schema->Server->Connection->Next($rs))
		{
			$this->Columns[$row["Field"]] = new DBColumn($this,$row);
		}

		$this->Schema->Server->Connection->Release($rs);
	}

	/**
	 * Load the keys and constraints for this table and populate the sets for
	 * all tables on which this table is dependents
	 *
	 * @access public
	 */
	public function LoadKeys()
	{

		// get the keys and constraints
		$sql = "show create table `".$this->Name."`";

		$create_table = "";

		$rs = $this->Schema->Server->Connection->Select($sql);

		if ($row = $this->Schema->Server->Connection->Next($rs))
		{
			if (isset($row["Create Table"]))
			{
				$create_table = $row["Create Table"];
			}
			else if (isset($row["Create View"]))
			{
				$this->IsView = true;
				$create_table = $row["Create View"];
				
				// treat the 1st column in a view as the primary key
				$this->Columns[$this->GetColumnNameByIndex(0,false)]->Key = 'PRI';
			}
			else
			{
				throw new Exception("Unknown Table Type");
			}
		}

		$this->Schema->Server->Connection->Release($rs);

		$lines = explode("\n", $create_table);

		foreach ($lines as $line)
		{
			$line = trim($line);
			if (substr($line,0,11) == "PRIMARY KEY")
			{
				preg_match_all("/`(\w+)`/",$line, $matches, PREG_PATTERN_ORDER);
				// print "<pre>";  print_r($matches);  die(); // DEBUG
				$this->PrimaryKeys[$matches[1][0]] = new DBKey($this,"PRIMARY KEY",$matches[0][0]);
			}
			elseif (substr($line,0,3) == "KEY")
			{
				preg_match_all("/`(\w+)`/",$line, $matches, PREG_PATTERN_ORDER);
				// print "<pre>";  print_r($matches);  die(); // DEBUG
				$this->ForeignKeys[$matches[1][0]] = new DBKey($this,$matches[1][0],$matches[1][1]);

				// Add keys to the column for convenience
				$this->Columns[$matches[1][1]]->Keys[] = $matches[1][0];
			}
			elseif (substr($line,0,10) == "CONSTRAINT")
			{
				preg_match_all("/`(\w+)`/",$line, $matches, PREG_PATTERN_ORDER);
				//print "<pre>";  print_r($matches);  die(); // DEBUG
				$this->Constraints[$matches[1][0]] = new DBConstraint($this,$matches[1]);

				// the set is basically the reverse of the constraint, but we want to add it to the
				// constraining table so we don't have to do reverse-lookup looking for child relationships
				$this->Schema->Tables[$matches[1][2]]->Sets[$matches[1][0]] = new DBSet($this,$matches[1]);

				//print "<pre>##########################\r\n" . print_r($matches,1) . "\r\n##########################\r\n";

				// Add constraints to the column for convenience
				$this->Columns[$matches[1][1]]->Constraints[] = $matches[1][0];
			}
			elseif ( strstr( $line, "COMMENT ") )
			{
				// TODO: this is pretty fragile... ?
				// table comments and column comments are seemingly differentiated by "COMMENT="  vs "COMMENT "
				$parts = explode("`",$line);
				$column = $parts[1];
				$comment = strstr( $line, "COMMENT ");
				$comment = substr($comment,9,strlen($comment)-11);
				$comment = str_replace("''","'",$comment);
				$this->Columns[$column]->Comment = $comment;

				if ($this->Columns[$column]->Default == "" && substr($comment,0,8) == "default=")
				{
					$this->Columns[$column]->Default = substr($comment,9, strlen($comment)-10 );
				}

				// print "<pre>" . $column . "=" . htmlspecialchars( $this->Columns[$column]->Default );

			}
			// TODO: look for COMMENT
		}
	}

}

?>