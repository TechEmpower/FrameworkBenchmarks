<?php
/** @package    verysimple::Phreeze */

/**
 * DataPage is a container for one "page" of data in a DataSet
 * This is used for displaying results in small chunks.  A DataPage
 * is returned by DataSet::GetDataPage
 *
 * @package    verysimple::Phreeze
 * @author     VerySimple Inc. <noreply@verysimple.com>
 * @copyright  1997-2007 VerySimple Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    1.1
 */
class DataPage implements Iterator
{
    /**
     * The Rows property is an array of objects retreived from the data store
     */
    public $Rows = null;

	/**
	 * ObjectName is the classname of the object that is stored
	 */
    public $ObjectName = "";

	/**
	 * ObjectInstance is an instance of the class that is stored in Rows
	 * @var Phreezable
	 */
    public $ObjectInstance = null;

	/**
	 * ObjectKey is the name of the primary key property for the objects in Rows
	 */
    public $ObjectKey = "";

    public $TotalResults = 0;
    public $TotalPages = 0;
    public $CurrentPage = 0;
    public $PageSize = 0;


    /**
     * @return Phreezable
     */
    public function Next()
    {
		return next($this->Rows);
	}

	public function rewind() {
		reset($this->Rows);
	}

	/**
	 * @return Phreezable
	 */
	public function current() {
		return current($this->Rows);
	}

	public function key() {
		return key($this->Rows);
	}

	public function valid() {
		return $this->current() !== false;
	}

	/**
	* Returns the entire page as an array of objects.  if the asSimpleObject is false
	* then the stateful Phreezable objects will be returned.  If asSimpleObject is true
	* then the objects returned will be whatever is returned by ToObject()
	* Phreezable object (the default is a stdClass with all public properties)
	*
	* @access public
    * @param bool asSimpleObject if true then populate the array with ToObject on each item in the array
    * @param array options (only relevant if asSimpleObject is true) passed through to ToObject
	* @return array
	    */
	function ToObjectArray($asSimpleObject = false, $options = null)
	{
		$arr = null;

		if ($asSimpleObject)
		{
			$arr = array();
			foreach ($this->Rows as $row)
			{
				$arr[] = $row->ToObject($options);
			}
		}
		else
		{
			$arr = $this->Rows;
		}

		return $arr;
	}
}

?>