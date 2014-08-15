<?php
/** @package    verysimple::Phreeze */

/**
 * QueryBuilder generates the actual SQL that is executed by Phreezer
 *
 * @package    verysimple::Phreeze
 * @author     VerySimple Inc.
 * @copyright  1997-2007 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    2.01
 */
 class QueryBuilder
{
	private $_phreezer;
	private $_counter = 0;
	public $Columns;
	public $Tables;
	public $Joins;

	/**
	 * Constructor
	 *
	 * @param Phreezer $phreezer persistance engine
	 */
	public function __construct($phreezer)
	{
		$this->_phreezer =& $phreezer;
		$this->Columns = Array();
		$this->Tables = Array();
		$this->Joins = Array();
	}
	
	/**
	 * Adds a field map to the queue, which will be used later when the SQL is generated
	 *
	 * @param FieldMap $fm
	 */
	public function AddFieldMap($fm)
	{
		$tablealias = $fm->TableName;
		if (!array_key_exists($tablealias, $this->Tables)) $this->Tables[$tablealias] = $fm->TableName;
		
		// debugging sequence of loading tables
		// print "<div>QueryBuilder->AddFieldMap:" . $tablealias . "-&gt;" . $fm->ColumnName . "</div>";
		
		$this->Columns[$tablealias ."-". $fm->ColumnName] = $fm->FieldType == FM_CALCULATION 
			? $fm->ColumnName 
			: "`" . $tablealias . "`.`" . $fm->ColumnName . "` as `" . $fm->ColumnName . "___" . $fm->TableName . "___" . $this->_counter++ . "`";
	}
	
	private $_keymapcache = array();  // used to check for recursive eager fetching
	private $_prevkeymap;  // used to check for recursive eager fetching
	
	/**
	 * Each Model that is to be included in this query is recursed, where all fields are added
	 * to the queue and then looks for eager joins to recurse through
	 *
	 * @param string $typename name of the Model object to be recursed
	 * @param FieldMap $fm
	 */
	public function RecurseFieldMaps($typename, $fms)
	{

		// if we see the same table again, we have an infinite loop
		if (isset($this->_keymapcache[$typename]))
		{
			// return;  // TODO: why doesn't this work..?
			throw new Exception("A circular EAGER join was detected while parsing `$typename`.  This is possibly due to an EAGER join with `".$this->_prevkeymap."`  Please edit your Map so that at least one side of the join is LAZY.");
		}

		// first we just add the basic columns of this object
		foreach ($fms as $fm)
		{
			$this->AddFieldMap($fm);
		}
		
		// get the keymaps for the requested object
		$kms = $this->_phreezer->GetKeyMaps($typename);
		$this->_keymapcache[$typename] = $kms;  // save this to prevent infinite loop
		$this->_prevkeymap = $typename;
		
		// each keymap in this object that is eagerly loaded, we want to join into the query.
		// each of these tables, then might have eagerly loaded keymaps as well, so we'll use
		// recursion to eagerly load as much of the graph as is desired
		foreach ($kms as $km)
		{
			if ($km->LoadType == KM_LOAD_EAGER || $km->LoadType == KM_LOAD_INNER)
			{
				// check that we didn't eagerly join this already.
				// TODO: use aliases to support multiple eager joins to the same table
				if (isset($this->Joins[$km->ForeignObject . "_is_joined"]))
				{
					//print_r($typename);
					throw new Exception($typename ."Map has multiple EAGER joins to `" . $km->ForeignObject . "` which is not yet supported by Phreeze.");
				}
				
				$ffms = $this->_phreezer->GetFieldMaps($km->ForeignObject);
				
				$this->RecurseFieldMaps($km->ForeignObject, $ffms);
				
				// lastly we need to add the join information for this foreign field map
				$jointype = $km->LoadType == KM_LOAD_INNER ? "inner" : "left";
				
				foreach ($ffms as $ffm)
				{
					if (!isset($this->Joins[$ffm->TableName]))
					{
						$this->Joins[$ffm->TableName] = " ".$jointype." join `".$ffm->TableName."` on `" . $fms[$km->KeyProperty]->TableName . "`.`" .  $fms[$km->KeyProperty]->ColumnName . "` = `" . $ffms[$km->ForeignKeyProperty]->TableName . "`.`" . $ffms[$km->ForeignKeyProperty]->ColumnName . "`";
					}

				}
				
				// keep track of what we have eagerly joined already
				$this->Joins[$km->ForeignObject . "_is_joined"] = 1;
			}
		}
	}
	
	/**
	 * Returns an array of column names that will be included in this query
	 *
	 * @return string comma-separated list of escaped DB column names
	 */
	public function GetColumnNames()
	{
		return implode(", ",array_values($this->Columns));
	}
	
	/**
	 * Determines what tables need to be included in the query 
	 * based on the mapping
	 * @param Criteria $criteria
	 * @return string "from" sql
	 */
	private function GetTableJoinSQL($criteria)
	{
		$sql = "";
		
		$tablenames = array_keys($this->Tables);
		
		if (count($tablenames) > 1)
		{
			// we're selecting from multiple tables so we have to do an outer join
			$sql .= " from `" . $tablenames[0] . "`";
				
			// WARNING: if tables are being joined in the incorrect order, it is likely
			// caused by a query that goes across more than one foreign relationship.
			// you can force tables to be joined in a specific order by adding a field
			// reference in the outermost Phreezable FieldMap.  QueryBuilder will
			// include tables in whatever order they are references, so you can control
			// this by simply adding in referenced to foreign field in the order you want
			// the tables to be joined
			//for ($i = count($tablenames) -1; $i > 0 ; $i--) // this iterates backwards
			for ($i = 1; $i < count($tablenames); $i++)      // this iterates forwards
			{
				try
				{
					$sql .= $this->Joins[$tablenames[$i]];
				}
				catch (Exception $ex)
				{
					// if 'undefined index' occurs here, there is likely a foreign field in the fieldmap that does not have it's related keymap set to KM_LOAD_EAGER
					throw new Exception("An invalid join was attempted from table '" . $tablenames[$i] 
						. "'. Please verify that the KeyMap fetching strategy for table '" . $tablenames[0]
						. "' has been properly configured.");
				}
			}
		}
		else
		{
			// we are only selecting from one table
			// (LL) added backticks here
			$sql .= " from `" . $tablenames[0] . "` ";
		}
		
		return $sql;
	}
	
	/**
	 * Removes the "where" from the beginning of a statement
	 * @param string partial query to parse
	 * @return string query without the preceeding "where"
	 */
	private function RemoveWherePrefix($sql)
	{
		$sql = trim($sql);
		
		// remove if the query is surrounded by parenths
		while (substr($sql,0,1) == "(" && substr($sql,0,-1) == ")")
		{
			$sql = trim( substr($sql,1,-1) );
		}
		
		while (strtolower( substr($sql, 0, 5) ) == "where")
		{
			$sql = trim(substr($sql,5));
		}
		
		return $sql;
	}
	
	/**
	 * Returns the "where" part of the query based on the criteria parameters
	 * @param Criteria $criteria
	 * @return String "where" part of the sql query
	 */
	private function GetWhereSQL($criteria)
	{
		$ands = $criteria->GetAnds();
		$ors = $criteria->GetOrs();
		
		// TODO: this all needs to move to the criteria object so it will recurse properly  ....
		$where = $this->RemoveWherePrefix($criteria->GetWhere());
		
		if (count($ands))
		{
			$wdelim = ($where) ? " and " : "";
			foreach($ands as $c)
			{
				$tmp = $c->GetWhere();
				$buff = $this->RemoveWherePrefix($tmp);
				if ($buff)
				{
					$where .= $wdelim . $buff;
					$wdelim = " and ";
				}
			}
		}
		
		if (count($ors))
		{
			$where = trim($where) ? "(" . $where . ")" : ""; // no primary criteria.  kinda strange
			$wdelim = $where ? " or " : "";
				
			foreach($ors as $c)
			{
				$tmp = $c->GetWhere();
				$buff = $this->RemoveWherePrefix($tmp);
				if ($buff)
				{
					$where .= $wdelim . "(" . $buff . ")";
					$wdelim = " or ";
				}
			}
		}
		
		// .. end of stuff that should be in criteria	

		// prepend the "where" onto the statement
		if ($where) $where = " where (" . trim($where) . ") ";
		
		return $where;
	}
	
	/**
	 * Builds a SQL statement from the given criteria object and resets the Criteria
	 *
	 * @param Criteria $criteria
	 * @return string fully formed SQL statement
	 */
	public function GetSQL($criteria)
	{
		// start building the sql statement
		$sql = "select " . $this->GetColumnNames() . "";

		$sql .= $this->GetTableJoinSQL($criteria);		

		$sql .= $criteria->GetJoin();
				
		$sql .= $this->GetWhereSQL($criteria);
		
		$sql .= $criteria->GetOrder();
		
		$criteria->Reset();
		
		return $sql;
	}
	
	/**
	* Builds a SQL statement from the given criteria object to count the results and resets the Criteria
	*
	* @param Criteria $criteria
	* @return string fully formed SQL statement
	*/
	public function GetCountSQL($criteria)
	{
		$sql = "select count(1) as counter ";
	
		$sql .= $this->GetTableJoinSQL($criteria);
	
		$sql .= $criteria->GetJoin();
	
		$sql .= $this->GetWhereSQL($criteria);
		
		$criteria->Reset();
	
		return $sql;
	}
}

?>