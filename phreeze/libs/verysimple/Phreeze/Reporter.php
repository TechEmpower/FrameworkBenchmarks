<?php
/** @package    verysimple::Phreeze */

/**
 * Reporter allows creating dynamic objects that do not necessarily reflect
 * the structure of the datastore table.  This is often useful for reporting
 * or returning aggregate data
 * @package    verysimple::Phreeze
 * @author     VerySimple Inc. <noreply@verysimple.com>
 * @copyright  1997-2005 VerySimple Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    1.0
 */
abstract class Reporter implements Serializable
{
    protected $_phreezer;

	private $_isLoaded;
	private $_isPartiallyLoaded;
	private $_cacheLevel = 0;
	private $_noCache = false;

	/** @var these properties will never be cached */
	private static $NoCacheProperties = array("_cache","_phreezer","_val_errors","_base_validation_complete");

	/** @var cache of public properties for each type for improved performance when enumerating */
	private static $PublicPropCache = array();

	/**
	* Returns true if the current object has been loaded
	* @access     public
	* @param      bool (optional) if provided will change the value
	* @return     bool
	*/
	public function IsLoaded($value = null)
	{
		if ($value != null) $this->_isLoaded = $value;
		return $this->_isLoaded;
	}

	/**
	* Returns true if the current object has been partially loaded
	* @access     public
	* @param      bool (optional) if provided will change the value
	* @return     bool
	*/
	public function IsPartiallyLoaded($value = null)
	{
		if ($value != null) $this->_isPartiallyLoaded = $value;
		return $this->_isPartiallyLoaded;
	}

	/**
	* Returns 0 if this was loaded from the DB, 1 if from 1st level cache and 2 if 2nd level cache
	* @access     public
	* @param      bool (optional) if provided will change the value
	* @return     bool
	*/
	public function CacheLevel($value = null)
	{
		if ($value != null) $this->_cacheLevel = $value;
		return $this->_cacheLevel;
	}

	/**
	* Returns true if the current object should never be cached
	* @access     public
	* @param      bool (optional) if provided will change the value
	* @return     bool
	*/
	public function NoCache($value = null)
	{
		if ($value != null) $this->_noCache = $value;
		return $this->_noCache;
	}


    /**
    * constructor
    *
    * @access     public
    * @param      Phreezer $phreezer
    * @param      Array $row
    */
    final function __construct(&$phreezer, $row = null)
    {
		$this->_phreezer = $phreezer;

        if ($row)
        {
            $this->Load($row);
        }
    }

    /**
	 * When serializing, make sure that we ommit certain properties that
	 * should never be cached or serialized.
	 */
	function serialize()
	{
		$propvals = array();
		$ro = new ReflectionObject($this);

		foreach ($ro->getProperties() as $rp )
		{
			$propname = $rp->getName();

			if (!in_array($propname,self::$NoCacheProperties))
			{
				if (method_exists($rp,"setAccessible"))
				{
					$rp->setAccessible(true);
					$propvals[$propname] = $rp->getValue($this);
				}
				elseif (!$rp->isPrivate())
				{
					// if < php 5.3 we can't serialize private vars
					$propvals[$propname] = $rp->getValue($this);
				}

			}
		}

		return serialize($propvals);
	}

	/**
	 * Reload the object when it awakes from serialization
	 * @param $data
	 */
	function unserialize($data)
	{
		$propvals = unserialize($data);

		$ro = new ReflectionObject($this);

		foreach ($ro->getProperties() as $rp )
		{
			$propname = $rp->name;
			if ( array_key_exists($propname,$propvals) )
			{
				if (method_exists($rp,"setAccessible"))
				{
					$rp->setAccessible(true);
					$rp->setValue($this,$propvals[$propname]);
				}
				elseif (!$rp->isPrivate())
				{
					// if < php 5.3 we can't serialize private vars
					$rp->setValue($this,$propvals[$propname]);
				}

			}
		}
	}

	/**
	* Returns an array with all public properties, excluding any internal
	* properties used by the Phreeze framework.  This is cached for performance
	* when enumerating through large numbers of the same class
	* @return array
	*/
	public function GetPublicProperties()
	{
		$className = get_class($this);

		if (!array_key_exists($className, self::$PublicPropCache))
		{

			$props = array();
			$ro = new ReflectionObject($this);

			foreach ($ro->getProperties() as $rp )
			{
				$propname = $rp->getName();

				if (!in_array($propname,self::$NoCacheProperties))
				{
					if (!($rp->isPrivate() || $rp->isStatic()))
					{
						$props[] = $propname;
					}
				}
			}

			self::$PublicPropCache[$className] = $props;
		}

		return self::$PublicPropCache[$className];
	}

	/**
	* Return an object with a limited number of properties from this Phreezable object.
	* This can be used if not all properties are necessary, for example rendering as JSON
	*
	* This can be overriden per class for custom JSON output.  the overridden method may accept
	* additional option parameters that are not supported by the base Phreezable calss
	*
	* @param array assoc array of options. This is passed through from Controller->RenderJSON
	* 		props (array) array of props to return (if null then use all public props)
	* 		omit (array) array of props to omit
	* 		camelCase (bool) if true then first letter of each property is made lowercase
	* @return stdClass
	*/
	function ToObject($options = null)
	{
		if ($options === null) $options = array();
		$props = array_key_exists('props', $options) ? $options['props'] : $this->GetPublicProperties();
		$omit = array_key_exists('omit', $options) ? $options['omit'] : array();
		$camelCase = array_key_exists('camelCase', $options) ? $options['camelCase'] : false;

		$obj = new stdClass();

		foreach ($props as $prop)
		{
			if (!in_array($prop, $omit))
			{
				$newProp = ($camelCase) ? lcfirst($prop) : $prop;
				$obj->$newProp = $this->$prop;
			}
		}

		return $obj;
	}

	/**
	 * Restores the object's connection to the datastore, for example after serialization
	 * @param $phreezer
	 * @param $row
	 */
	function Refresh(Phreezer $phreezer, $row = null)
	{
		$this->_phreezer = $phreezer;

		if ($row)
        {
            $this->Load($row);
        }

         $this->OnRefresh();
	}

	/**
    * Called after object is refreshed, may be overridden
    *
    * @access     public
    */
    public function OnRefresh(){}

    /**
    * This static function can be overridden to populate this object with
    * results of a custom query
    *
    * @access     public
    * @param      Criteria $criteria
    * @return     string
    */
    static function GetCustomQuery($criteria)
    {
    	return "";
    }

    /**
	 * This may be overridden to return SQL used for counting the number of rows
	 * in a result.  This method is not required, however it will allow 
	 * Phreeze to use an efficient query for counting results.  This query 
	 * must return the correct number of results that GetCustomQuery would, 
	 * given the same criteria
	 * 
	 * The resultant SQL must return only one row with one column named 'counter'
    *
    * @access     public
    * @param      Criteria $criteria
    * @return     string
    */
    static function GetCustomCountQuery($criteria)
    {
    	return "";
    }

	/**
    * Returns this object as an associative array with properties as keys and
    * values as values
    *
    * @access     public
    * @return     array
    */
    function GetArray()
    {
		$fms = $this->_phreezer->GetFieldMaps(get_class($this));
		$cols = Array();

        foreach ($fms as $fm)
        {
			$prop = $fm->PropertyName;
			$cols[$fm->ColumnName] = $this->$prop;
        }

        return $cols;
	}

    /**
    * Loads the object with data given in the row array.
    *
    * @access     public
    * @param      Array $row
    */
    function Load(&$row)
    {
		$this->_phreezer->Observe("Loading " . get_class($this),OBSERVE_DEBUG);

		foreach (array_keys($row) as $prop)
		{
			$this->$prop = $row[$prop];
		}

        $this->OnLoad();
    }

    /**
    * Called after object is loaded, may be overridden
    *
    * @access     protected
    */
    protected function OnLoad(){}

}

?>