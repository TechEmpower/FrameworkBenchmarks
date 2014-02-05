<?php
/** @package    verysimple::Phreeze */

/** import supporting libraries */
require_once("DataAdapter.php");
require_once("CriteriaFilter.php");
require_once("verysimple/IO/Includer.php");

/**
 * Criteria is a base object that is passed into Phreeze->Query for retreiving
 * records based on specific criteria
 *
 * @package    verysimple::Phreeze
 * @author     VerySimple Inc.
 * @copyright  1997-2007 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    2.3
 */
class Criteria
{
	protected $_join;
	protected $_where;
	protected $_where_delim;
	protected $_order;
	protected $_order_delim;
	protected $_is_prepared;

	protected $_map_object_class;
	
	private $_fieldmaps;
	private $_keymaps;
	
	private $_constructor_where;
	private $_constructor_order;
	private $_set_order;
	
	private $_and = array();
	private $_or = array();
	
	public $PrimaryKeyField;
	public $PrimaryKeyValue;
	
	/**
	 * @var $Filters a CriteriaFilter or array of CriteriaFilters to be applied to the query
	 */
	public $Filters;
	
	public function __construct($where = "", $order = "")
	{
		$this->_constructor_where = $where;
		$this->_constructor_order = $order;
		
		$this->_where = $where;
		$this->_order = $order;
		
		$this->Init();
	}
	
	/**
	 * Init is called directly after construction and can be overridden.  If the
	 * name of the Criteria class is not ObjectClassCriteria, then this method
	 * must be overriden and _map_object_class should be set to the correct
	 * name of the DAO Map class
	 */
	protected function Init()
	{
		$this->_map_object_class = str_replace("Criteria","Map",get_class($this));
	}
	
	/**
	 * Add a CriteriaFilter to the criteria for custom filtering of results
	 * @param CriteriaFilter $filter
	 */
	public function AddFilter(CriteriaFilter $filter)
	{
		if (!$this->Filters) $this->Filters = array();
		$this->Filters[] = $filter;
	}

	/**
	 * Return an array of CriteriaFilters that have been added to this criteria
	 * @return array
	 */
	public function GetFilters()
	{
		return $this->Filters;
	}
	
	/**
	 * Remove all filters that are currently attached
	 */
	public function ClearFilters()
	{
		$this->Filters = null;
	}
	
	/**
	 * Adds a criteria to be joined w/ an "and" statement.
	 * Criterias to foreign objects may be added as long as they
	 * have an immediate relationship to the foreign table
	 * 
	 * @param Criteria
	 * @param string [optional] id of the foreign key map. If the same table is joined
	 * multiple times, then you should specify which keymap to use
	 */
	public function AddAnd(Criteria $criteria, $keymap_id = null)
	{
		$this->_and[] = $criteria;
	}
		
	/**
	 * Return any and criterias that have been added to this criteria
	 * @return array
	 */
	public function GetAnds()
	{
		return $this->_and;
	}
	
	/**
	 * Escape values for insertion into a SQL query string
	 * @return string
	 */
	public function Escape($val)
	{
		return DataAdapter::Escape($val);
	}

	/**
	 * Returns DataAdapter::GetQuotedSql($val)
	 * @param variant $val to be quoted
	 * @return string
	 */
	public function GetQuotedSql($val)
	{
		return DataAdapter::GetQuotedSql($val);
	}
	
	/**
	 * Adds a criteria to be joined w/ an "or" statement.
	 * Criterias to foreign objects may be added as long as they
	 * have an immediate relationship to the foreign table
	 * 
	 * @param Criteria
	 * @param string [optional] id of the foreign key map. If the same table is joined
	 * multiple times, then you should specify which keymap to use
	 */
	public function AddOr(Criteria $criteria, $keymap_id = null)
	{
		$this->_or[] = $criteria;
	}
	
	/**
	 * Return any 'or' criterias that have been added to this criteria
	 * @return array
	 */
	public function GetOrs()
	{
		return $this->_or;
	}
	
	/**
	 * Reset the Criteria for re-use.  This is called by querybuilder after the criteria has been used
	 * to generate SQL.  It can be called manually as well.
	 */
	public function Reset()
	{
		$this->_is_prepared = false;
		$this->_where = $this->_constructor_where;
		$this->_order = $this->_constructor_order;
	}
	
	/** Prepare is called just prior to execution and will fire OnPrepare after it completes
	 * If this is a base Criteria class, then we can only do a lookup by PrimaryKeyField or
	 * else raw SQL must be provided during construction.  _Equals, _BeginsWith can only be
	 * used by inherited Criteria classes because we don't know what table this is associated
	 * with, so we can't translate property names to column names.
	 *
	 */
	private final function Prepare()
	{
		if (!$this->_is_prepared)
		{
		
			if (get_class($this) == "Criteria")
			{
				if ($this->PrimaryKeyField)
				{
					// PrimaryKeyField property was specified. this might be coming from $phreezer->Get
					$this->_where = " " . $this->PrimaryKeyField ." = '". $this->Escape($this->PrimaryKeyValue) . "'";
				}
				// else {raw SQL was likely provided in the constructor. this might be coming from $phreezer->GetOneToMany}
			}
			else
			{
				// loop through all of the properties and attempt to 
				// build a query based on any values that have been set
				$this->_where = '';
				$this->_where_delim = '';
				
				$props = get_object_vars($this);
				foreach ($props as $prop => $val)
				{
					// TODO: tighten this up a bit to reduce redundant code
					if ($prop == "Filters" && isset($val) && (is_array($val) || is_a($val, 'CriteriaFilter')) )
					{
						// a filter object will take care of generating it's own where statement
						
						// normalize the input to accept either an individual filter or multiple filters
						$filters = (is_array($val)) ? $val : array($val);
						
						foreach ($filters as $filter)
						{
							$this->_where .= $this->_where_delim . ' ' . $filter->GetWhere($this);
							$this->_where_delim = " and";
						}
					}
					elseif (substr($prop,-7) == "_Equals" && strlen($this->$prop))
					{
						$dbfield = $this->GetFieldFromProp(str_replace("_Equals","",$prop));
						$this->_where .= $this->_where_delim . " " . $dbfield ." = ". $this->GetQuotedSql($val) . "";
						$this->_where_delim = " and";
					}
					elseif (substr($prop,-10) == "_NotEquals" && strlen($this->$prop))
					{
						$dbfield = $this->GetFieldFromProp(str_replace("_NotEquals","",$prop));
						$this->_where .= $this->_where_delim . " " . $dbfield ." != ". $this->GetQuotedSql($val) . "";
						$this->_where_delim = " and";
					}
					elseif (substr($prop,-8) == "_IsEmpty" && $this->$prop)
					{
						$dbfield = $this->GetFieldFromProp(str_replace("_IsEmpty","",$prop));
						$this->_where .= $this->_where_delim . " " . $dbfield ." = ''";
						$this->_where_delim = " and";
					}
					elseif (substr($prop,-11) == "_IsNotEmpty" && $this->$prop)
					{
						$dbfield = $this->GetFieldFromProp(str_replace("_IsNotEmpty","",$prop));
						$this->_where .= $this->_where_delim . " " . $dbfield ." != ''";
						$this->_where_delim = " and";
					}
					elseif (substr($prop,-7) == "_IsLike" && strlen($this->$prop))
					{
						$dbfield = $this->GetFieldFromProp(str_replace("_IsLike","",$prop));
						$this->_where .= $this->_where_delim . " " . $dbfield ." like '%". $this->Escape($val) . "%'";
						$this->_where_delim = " and";
					}
					elseif (substr($prop,-10) == "_IsNotLike" && strlen($this->$prop))
					{
						$dbfield = $this->GetFieldFromProp(str_replace("_IsNotLike","",$prop));
						$this->_where .= $this->_where_delim . " " . $dbfield ." not like '%". $this->Escape($val) . "%'";
						$this->_where_delim = " and";
					}
					elseif (substr($prop,-11) == "_BeginsWith" && strlen($this->$prop))
					{
						$dbfield = $this->GetFieldFromProp(str_replace("_BeginsWith","",$prop));
						$this->_where .= $this->_where_delim . " " . $dbfield ." like '". $this->Escape($val) . "%'";
						$this->_where_delim = " and";
					}
					elseif (substr($prop,-9) == "_EndsWith" && strlen($this->$prop))
					{
						$dbfield = $this->GetFieldFromProp(str_replace("_EndsWith","",$prop));
						$this->_where .= $this->_where_delim . " " . $dbfield ." like '%". $this->Escape($val) . "'";
						$this->_where_delim = " and";
					}
					elseif (substr($prop,-12) == "_GreaterThan" && strlen($this->$prop))
					{
						$dbfield = $this->GetFieldFromProp(str_replace("_GreaterThan","",$prop));
						$this->_where .= $this->_where_delim . " " . $dbfield ." > ". $this->GetQuotedSql($val) . "";
						$this->_where_delim = " and";
					}
					elseif (substr($prop,-19) == "_GreaterThanOrEqual" && strlen($this->$prop))
					{
						$dbfield = $this->GetFieldFromProp(str_replace("_GreaterThanOrEqual","",$prop));
						$this->_where .= $this->_where_delim . " " . $dbfield ." >= ". $this->GetQuotedSql($val) . "";
						$this->_where_delim = " and";
					}
					elseif (substr($prop,-9) == "_LessThan" && strlen($this->$prop))
					{
						$dbfield = $this->GetFieldFromProp(str_replace("_LessThan","",$prop));
						$this->_where .= $this->_where_delim . " " . $dbfield ." < ". $this->GetQuotedSql($val) . "";
						$this->_where_delim = " and";
					}
					elseif (substr($prop,-16) == "_LessThanOrEqual" && strlen($this->$prop))
					{
						$dbfield = $this->GetFieldFromProp(str_replace("_LessThanOrEqual","",$prop));
						$this->_where .= $this->_where_delim . " " . $dbfield ." <= ". $this->GetQuotedSql($val) . "";
						$this->_where_delim = " and";
					}
					elseif (substr($prop,-10) == "_BitwiseOr" && strlen($this->$prop))
					{
						$dbfield = $this->GetFieldFromProp(str_replace("_BitwiseOr","",$prop));
						$this->_where .= $this->_where_delim . " (" . $dbfield ." | '". $this->Escape($val) . ")";
						$this->_where_delim = " and";
					}
					elseif (substr($prop,-11) == "_BitwiseAnd" && strlen($this->$prop))
					{
						$dbfield = $this->GetFieldFromProp(str_replace("_BitwiseAnd","",$prop));
						$this->_where .= $this->_where_delim . " (" . $dbfield ." & ". $this->Escape($val) . ")";
						$this->_where_delim = " and";
					}
					elseif (substr($prop,-16) == "_LiteralFunction" && strlen($this->$prop))
					{
						$dbfield = $this->GetFieldFromProp(str_replace("_LiteralFunction","",$prop));
						$this->_where .= $this->_where_delim . " (" . $dbfield ." ". $val . ")";
						$this->_where_delim = " and";
					}
					elseif (substr($prop,-3) == "_In" && isset($val))
					{
						// if a string was passed in then treat it as comma-delimited
						if  (!is_array($val)) $val = explode(',', $val);
						
						// if the count is zero, technically the user is saying that they don't
						// want any results.  the only way to do that is to make the criteria
						// something that will for sure not match any existing records.  we cannot
						// 100% guarantee this, though, we can choose a highly unlikely value
						// that will never return a match under ordinary circumstances
						if (count($val) == 0)
						{
							array_push($val,"$prop EMPTY PHREEZE CRITERIA ARRAY");
						}
						
						$dbfield = $this->GetFieldFromProp(str_replace("_In","",$prop));
						$this->_where .= $this->_where_delim . " " . $dbfield ." in (";
						$indelim = "";
						foreach ($val as $n)
						{ 
							$this->_where .= $indelim . "'" . $this->Escape($n) . "'";
							$indelim = ",";
						}
						$this->_where .= ")";
						$this->_where_delim = " and";
					}
					elseif (substr($prop,-6) == "_NotIn" && isset($val))
					{
						// if a string was passed in then treat it as comma-delimited
						if  (!is_array($val)) $val = explode(',', $val);
						
						// if the count is zero, technically the user is saying that they don't
						// want any results.  the only way to do that is to make the criteria
						// something that will for sure not match any existing records.  we cannot
						// 100% guarantee this, though, we can choose a highly unlikely value
						// that will never return a match under ordinary circumstances
						if (count($val) == 0)
						{
							array_push($val,"$prop EMPTY PHREEZE CRITERIA ARRAY");
						}
						
						$dbfield = $this->GetFieldFromProp(str_replace("_NotIn","",$prop));
						$this->_where .= $this->_where_delim . " " . $dbfield ." not in (";
						$indelim = "";
						foreach ($val as $n)
						{ 
							$this->_where .= $indelim . "'" . $this->Escape($n) . "'";
							$indelim = ",";
						}
						$this->_where .= ")";
						$this->_where_delim = " and";
					}
				}
			}

			// prepend the sql so the statement will work correctly
			if ($this->_where)
			{
				$this->_where = " where " . $this->_where;
			}
			
			// if the user has called SetOrder then use that for the order
			if ($this->_set_order)
			{
				$this->_order = $this->_set_order;	
			}

			// if any of the filters have an order by then add those
			if (is_array($this->Filters)) {
				$orderDelim = $this->_order ? ',' : '';
				foreach ($this->Filters as $filter)
				{
					$filterOrder = $filter->GetOrder($this);
					if ($filterOrder) {
						$this->_order .= $orderDelim . $filterOrder;
						$orderDelim = ', ';
					}
				}
			}
			
			if ($this->_order)
			{
				$this->_order = " order by " . $this->_order;
			}

			$this->OnPrepare();
			$this->_is_prepared = true;
		}
	}
	
	public function OnPrepare() {}

	public final function GetWhere()
	{
		$this->Prepare();
		return $this->_where;
	}
	

	public final function GetJoin()
	{
		$this->Prepare();
		return $this->_join;
	}
	
	public final function GetOrder()
	{
		$this->Prepare();
		return $this->_order;
	}

	/**
	 * Adds an object property to the order by clause.  If any sorting needs to be done
	 * on foreign tables, then for the moment, you need to override this method and
	 * handle it manually.  You can call this method repeatedly to add more than
	 * one property for sorting.
	 *
	 * @param string $property the name of the object property (or '?' for random order)
	 * @param bool $desc (optional) set to true to sort in descending order (default false)
	 */
	public function SetOrder($property,$desc = false)
	{
		if (!$property)
		{
			// no property was specified.
			return;
		}
		
		$this->_order_delim = ($this->_set_order) ? "," : "";
		
		if($property == '?')
		{
			$this->_set_order = "RAND()" . $this->_order_delim . $this->_set_order;
		}
		else
		{
			$colname = $this->GetFieldFromProp($property);
			$this->_set_order .= $this->_order_delim . $colname . ($desc ? " desc" : "");	
		}

	}
	
	private function InitMaps()
	{
		if (!$this->_fieldmaps)
		{
			// we have to open the file to get the fieldmaps
			$mapname = $this->_map_object_class;
			$this->IncludeMap($mapname);
			
			$this->_fieldmaps = call_user_func(array($mapname,"GetFieldMaps"));
			$this->_keymaps = call_user_func(array($mapname,"GetKeyMaps"));
			
		}
	}
	
	
	/**
	* If the map class is not already defined, attempts to require_once the definition.
	* If the Map file cannot be located, an exception is thrown
	*
	* @access public
	* @param string $objectclass The name of the object map class
	*/
	public function IncludeMap($objectclass)
	{
		try
		{
			Includer::RequireClass($objectclass,"Model/DAO/");
		}
		catch (IncludeException $ex)
		{
			throw new Exception($ex->getMessage() . '.  If a map file does not exist then ' . get_class($this) . ' can implement GetFieldFromProp instead.');
		}
	}
	
	protected function GetFieldMaps()
	{
		$this->InitMaps();
		return $this->_fieldmaps;
	}
	
	protected function GetKeyMaps()
	{
		$this->InitMaps();
		return $this->_keymaps;
	}
	

	public function GetFieldFromProp($propname)
	{
		if (get_class($this) == "Criteria")
		{
			throw new Exception("Phreeze is unable to determine field mapping.  The base Criteria class should only be used to query by primary key without sorting");
		}

		$fms = $this->GetFieldMaps();
				
		// make sure this property is defined
		if (!isset($fms[$propname]))
		{
			throw new Exception(get_class($this) . " is unable to determine the database column for the property: '$propname'");
		}
		//print_r($this->_fieldmaps);
		$fm = $fms[$propname];
		
		return $fm->FieldType == FM_CALCULATION ? "(" . $fm->ColumnName . ")" : "`" . $fm->TableName . "`.`" . $fm->ColumnName . "`";

	}
}

?>