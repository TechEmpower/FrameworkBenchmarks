<?php
/** @package    verysimple::Phreeze */

/** import supporting libraries */


/**
 * CriteriaFilter allows arbitrary filtering based on one or more fields
 *
 * @package    verysimple::Phreeze
 * @author     VerySimple Inc.
 * @copyright  1997-2007 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    2.2
 */
class CriteriaFilter
{
	static $TYPE_SEARCH = 1;
	
	public $propertyNames;
	public $Value;
	public $Type;
	
	/**
	 * 
	 * @param variant $propertyNames comma-delimited string or array of property names (ie haystack)
	 * @param string $value search term (ie needle)
	 * @param int $type (default CriteriaFilter::TYPE_SEARCH)
	 */
	public function __construct($propertyNames,$value,$type = null)
	{
		$this->PropertyNames = $propertyNames;
		$this->Value = $value;
		$this->Type = ($type == null) ? self::$TYPE_SEARCH : $type;
	}
	
	/**
	 * Return the "where" portion of the SQL statement (without the where prefix)
	 * @param Criteria $criteria the Criteria object to which this filter has been added
	 */
	public function GetWhere($criteria)
	{
		if ($this->Type != self::$TYPE_SEARCH) throw new Exception('Unsupported Filter Type');
		
		// normalize property names as an array
		$propertyNames = (is_array($this->PropertyNames)) ? $this->PropertyNames : explode(',', $this->PropertyNames);
		
		$where = ' (';
		$orDelim = '';
		foreach ($propertyNames as $propName)
		{
			$dbfield = $criteria->GetFieldFromProp($propName);
			$where .= $orDelim . $criteria->Escape($dbfield) ." like ". $criteria->GetQuotedSql($this->Value) . "";
			$orDelim = ' or ';
		}
		$where .= ') ';
		
		return $where;
	}
	
	/**
	 * Return the "order by" portion of the SQL statement (without the order by prefix)
	 * @param Criteria $criteria the Criteria object to which this filter has been added
	 */
	public function GetOrder($criteria)
	{
		return "";
	}
}

?>