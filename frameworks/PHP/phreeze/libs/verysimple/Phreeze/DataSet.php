<?php
/** @package    verysimple::Phreeze */

/** import supporting libraries */
require_once("DataPage.php");

/**
 * DataSet stores zero or more Loadable objects
 * The DataSet is the object that is returned by every Phreezer Query operation.
 * The DataSet contains various methods to enumerate through , or retrieve all
 * results all at once.
 *
 * The DataSet executes queries lazily, only when the first result is retrieved.
 * Using GetDataPage will allow retreival of sub-sets of large amounts of data without
 * querying the entire database
 *
 * @package    verysimple::Phreeze
 * @author     VerySimple Inc. <noreply@verysimple.com>
 * @copyright  1997-2007 VerySimple Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    1.2
 */
class DataSet implements Iterator // @TODO implement Countable, ArrayAccess
{
    protected $_phreezer;
    protected $_rs;
    protected $_objectclass;
    protected $_counter;

    private $_sql;
    private $_current; // the current object in the set
	private $_last; // the previous object in the set
	private $_totalcount;
	private $_no_exception;  // used during iteration to suppress exception on the final Next call
	private $_cache_timeout;  // length of time to cache query results

	public $UnableToCache = true;

	/**
	 * A custom SQL query may be provided to count the results of the query.
	 * This query should return one column "counter" which is the number of rows
	 * and must take into account all criteria parameters.
	 * If no value is provided, the counter query will be generated (which is likely less efficient)
	 *
	 * @var string
	 */
	public $CountSQL = "";

    /**
    * Contructor initializes the object
    *
    * @access     public
    * @param Phreezer
    * @param string class of object this DataSet contains
    * @param string $sql code
    * @param int cache timeout (in seconds).  Default is Phreezer->ValueCacheTimeout.  Set to 0 for no cache
    */
    function __construct(&$preezer, $objectclass, $sql, $cache_timeout = null)
    {
        $this->_counter = -1;
		$this->_totalcount = -1;
		$this->_eof = false;
        $this->_objectclass = $objectclass;
        $this->_phreezer =& $preezer;
        $this->_rs = null;
        $this->_sql = $sql;
        $this->_cache_timeout = is_null($cache_timeout) ? $preezer->ValueCacheTimeout : $cache_timeout;
    }

    /**
    * _getObject must be overridden and returns the type of object that
    * this collection will contain.
    *
    * @access     private
    * @param      array $row array to use for populating a single object
    * @return     Preezable
    */
    private function _getObject(&$row)
    {
        $obj = new $this->_objectclass($this->_phreezer, $row);
        return $obj;
    }

    /**
    * Next returns the next object in the collection.
    *
    * @access     public
    * @return     Preezable
    */
    function Next()
    {
    	if ($this->UnableToCache)
    	{
    		require_once("verysimple/Util/ExceptionFormatter.php");
    		$info = ExceptionFormatter::FormatTrace(debug_backtrace());
    		$this->_phreezer->Observe("(DataSet.Next: unable to cache query with cursor) " . $info . "  " . $this->_sql,OBSERVE_DEBUG);

    		// use this line to discover where an uncachable query is coming from
    		// throw new Exception("WTF");

    		// stop this warning from repeating on every next call for this dataset
    	    $this->UnableToCache = false;
    	}

        $this->_verifyRs();

		$this->_current = null;
		$this->_counter++;

        if ($this->_eof)
        {
			if (!$this->_no_exception)
				throw new Exception("EOF: This is a forward-only dataset.");
        }

        if ($row = $this->_phreezer->DataAdapter->Fetch($this->_rs))
        {
            $this->_current = $this->_getObject($row);
			$this->_last = $this->_current;
		}
        else
        {
            $this->_eof = true;
        }

		return $this->_current;
    }


    /**
    * Executes the sql statement and fills the resultset if necessary
    */
    private function _verifyRs()
    {
        if ($this->_rs == null)
        {
			$this->_phreezer->IncludeModel($this->_objectclass);
			$this->_rs = $this->_phreezer->DataAdapter->Select($this->_sql);
        }
    }

    /**
     * If a reporter query does not return data (insert/update/delete) then
     * calling Execute will execute the sql without expecting return data
     */
    public function Execute()
    {
    	return $this->_phreezer->DataAdapter->Execute($this->_sql);
    }

	public function rewind() {
		$this->_rs = null;
		$this->_counter = 0;
		$this->_no_exception = true;
		$this->_total = $this->Count();
		$this->_verifyRs();
		$this->Next(); // we have to get the party started for php iteration
	}

	public function current() {
		// php iteration calls next then gets the current record.  The DataSet
		// Next return the current object.  so, we have to fudge a little on the
		// laster iteration to make it work properly
		return ($this->key() == $this->Count()) ? $this->_last : $this->_current;
	}

	public function key() {
		return $this->_counter;
	}

	public function valid() {
		return $this->key() <= $this->Count();
	}

	/**
	 * Returns true if the total number of records is known.  Because calling "Count"
	 * directly may fire a database query, this method can be used to tell if
	 * the number of records is known without actually firing any queries
	 * @return boolean
	 */
	function CountIsKnown()
	{
		return $this->_totalcount > -1;
	}

    /**
    * Count returns the number of objects in the collection.  If the
    * count is not available, a count statement will be executed to determine the total
    * number of rows available
    *
    * Note: if you get an "Unknown Column" error during a query, it may be due to tables being
    * joined in the wrong order.  To fix this, simply include references in your FieldMap to
    * the foreign tables in the same order that you wish them to be included in the query
    *
    * @access     public
    * @return     int
    */
    function Count()
    {
		if (!$this->CountIsKnown())
		{
			// check the cache
			$cachekey = $this->_sql . " COUNT";
			$this->_totalcount = $this->GetDelayedCache($cachekey);

			// if no cache, go to the db
			if ($this->_totalcount != null)
			{
				$this->_phreezer->Observe("DataSet.Count: skipping count query because cache exists",OBSERVE_DEBUG);
			}
			else
			{
				$this->LockCache($cachekey);

				
				$sql = "";

				// if a custom counter sql query was provided, use that because it should be more efficient
				if ($this->CountSQL)
				{
					$this->_phreezer->Observe("DataSet.Count: using CountSQL to obtain total number of records",OBSERVE_DEBUG);
					$sql = $this->CountSQL;
				}
				else
				{
					$this->_phreezer->Observe("(DataSet.Count: CountSQL was not provided so a counter query will be generated.  Implement GetCustomCountQuery in the reporter class to improve performance.)",OBSERVE_WARN);
					$sql = "select count(1) as counter from (" . $this->_sql . ") tmptable" . rand(1000,9999);
				}

				$rs = $this->_phreezer->DataAdapter->Select($sql);
				$row = $this->_phreezer->DataAdapter->Fetch($rs);
				$this->_phreezer->DataAdapter->Release($rs);
				$this->_totalcount = $row["counter"];

				$this->_phreezer->SetValueCache($cachekey, $this->_totalcount, $this->_cache_timeout);

				$this->UnlockCache($cachekey);
			}
		}

		return $this->_totalcount;
    }

    /**
    * Returns the entire collection as an array of objects.  if the asSimpleObject is false
	* then the stateful Phreezable objects will be returned.  If asSimpleObject is true
	* then the objects returned will be whatever is returned by ToObject() on each
	* Phreezable object (the default is a stdClass with all public properties)
    *
    * @access public
    * @param bool asSimpleObject if true then populate the array with ToObject()
    * @param array options (only relevant if asSimpleObject is true) passed through to ToObject
    * @return array
    */
    function ToObjectArray($asSimpleObject = false, $options = null)
    {
		$cachekey = $this->_sql . " OBJECTARRAY" . ($asSimpleObject ? '-AS-OBJECT-' .  serialize($options) : '');

		$arr = $this->GetDelayedCache($cachekey);

		if ($arr != null)
		{
			// we have a cache value, so we will repopulate from that
			$this->_phreezer->Observe("(DataSet.ToObjectArray: skipping query because cache exists) " . $this->_sql,OBSERVE_DEBUG);
			if (!$asSimpleObject)
			{
				foreach ($arr as $obj)
				{
					$obj->Refresh($this->_phreezer);
				}
			}
		}
		else
		{
			// there is nothing in the cache so we have to reload it

			$this->LockCache($cachekey);

			$this->UnableToCache = false;

			// use a fixed count array if the count is known for performance
			$arr = $this->CountIsKnown()
				? $this->GetEmptyArray($this->Count())
				: Array();

			$i = 0;
			while ($object = $this->Next())
			{
				$arr[$i++] = $asSimpleObject ? $object->ToObject($options) : $object;
			}

			$this->_phreezer->SetValueCache($cachekey, $arr, $this->_cache_timeout);

			$this->UnlockCache($cachekey);
		}

		return $arr;
    }


    /**
    * @deprecated Use GetLabelArray instead
    */
    function ToLabelArray($val_prop, $label_prop)
    {
        return $this->GetLabelArray($val_prop, $label_prop);
    }

    /**
     * Returns an empty array structure, determining which is appropriate
     * based on the system capabilities and whether a count is known.
     * If the count parameter is provided then the returned array may be
     * a fixed-size array (depending on php version)
     *
     * @param int count (if known)
     * @return Array or SplFixedArray
     */
    private function GetEmptyArray($count = 0)
    {
    	return ($count && class_exists('SplFixedArray'))
    		? new SplFixedArray($count)
    		: array();
    }

	/**
    * Returns the entire collection as an associative array that can be easily used
    * for Smarty dropdowns
    *
    * @access     public
    * @param      string $val_prop the object property to be used for the dropdown value
    * @param      string $label_prop the object property to be used for the dropdown label
    * @return     array
    */
    function GetLabelArray($val_prop, $label_prop)
    {
		// check the cache
		// $cachekey = md5($this->_sql . " VAL=".$val_prop." LABEL=" . $label_prop);
		$cachekey = $this->_sql . " VAL=".$val_prop." LABEL=" . $label_prop;

		$arr = $this->GetDelayedCache($cachekey);

		// if no cache, go to the db
		if ($arr != null)
		{
			$this->_phreezer->Observe("(DataSet.GetLabelArray: skipping query because cache exists) " . $this->_sql,OBSERVE_QUERY);
		}
		else
		{
			$this->LockCache($cachekey);

			$arr = Array();
			$this->UnableToCache = false;

			while ($object = $this->Next())
			{
				$arr[$object->$val_prop] = $object->$label_prop;
			}

			$this->_phreezer->SetValueCache($cachekey, $arr, $this->_cache_timeout);

			$this->UnlockCache($cachekey);
		}

        return $arr;
    }

	/**
	* Release the resources held by this DataSet
	*
	* @access     public
	*/
	function Clear()
    {
         $this->_phreezer->DataAdapter->Release($this->_rs);
    }

    /**
     * Returns a DataPage object suitable for binding to the smarty PageView plugin.
     * If $countrecords is true then the total number of records will be eagerly fetched
     * using a count query.  This is necessary in order to calculate the total number of
     * results and total number of pages.  If you do not care about pagination and simply
     * want to limit the results, then this can be set to false to supress the count 
     * query.  However, the pagination settings will not be correct and the total number
     * of rows will be -1
     *
     * @access     public
     * @param int $pagenum which page of the results to view
     * @param int $pagesize the size of the page (or zero to disable paging).
     * @param bool $countrecords will eagerly fetch the total number of records with a count query
     * @return DataPage
     */
    function GetDataPage($pagenum, $pagesize, $countrecords = true)
    {
		// check the cache
		// $cachekey = md5($this->_sql . " PAGE=".$pagenum." SIZE=" . $pagesize);
		$cachekey = $this->_sql . " PAGE=".$pagenum." SIZE=" . $pagesize." COUNT=" . ($countrecords ? '1' : '0');

		$page = $this->GetDelayedCache($cachekey);

		// if no cache, go to the db
		if ($page != null)
		{
			$this->_phreezer->Observe("(DataSet.GetDataPage: skipping query because cache exists) " . $this->_sql,OBSERVE_QUERY);

			foreach ($page->Rows as $obj)
			{
				$obj->Refresh($this->_phreezer);
			}
		}
		else
		{
			$this->LockCache($cachekey);

			$this->UnableToCache = false;

			$page = new DataPage();
			$page->ObjectName = $this->_objectclass;
			$page->ObjectInstance = new $this->_objectclass($this->_phreezer);
			$page->PageSize = $pagesize;
			$page->CurrentPage = $pagenum;
			
			if ($countrecords) 
			{
				$page->TotalResults = $this->Count();
	
				// first check if we have less than or exactly the same number of
				// results as the pagesize.  if so, don't bother doing the math.
				// we know we just have one page
				if ($page->TotalPages > 0 && $page->TotalPages <= $page->PageSize)
				{
					$page->TotalPages = 1;
				}
				else if ($pagesize == 0)
				{
					// we don't want paging to occur in this case
					$page->TotalPages = 1;
				}
				else
				{
					// we have more than one page.  we always need to round up
					// here because 5.1 pages means we are spilling out into
					// a 6th page.  (this will also handle zero results properly)
					$page->TotalPages = ceil( $page->TotalResults / $pagesize );
				}
			}
			else 
			{
				$page->TotalResults = $pagesize; // this will get adjusted after we run the query
				$page->TotalPages = 1;
			}

			// now enumerate through the rows in the page that we want.
			// decrement the requested pagenum here so that we will be
			// using a zero-based array - which saves us from having to
			// decrement on every iteration
			$pagenum--;

			$start = $pagesize * $pagenum;

			// @TODO the limit statement should come from the DataAdapter
			// ~~~ more efficient method where we limit the data queried ~~~
			// since we are doing paging, we want to get only the records that we
			// want from the database, so we wrap the original query with a
			// limit query.
			// $sql = "select * from (" . $this->_sql . ") page limit $start,$pagesize";
			$sql = $this->_sql . ($pagesize == 0 ? "" : " limit $start,$pagesize");
			$this->_rs = $this->_phreezer->DataAdapter->Select($sql);

	        // if we know the number of rows we have, then use SplFixedArray for performance
			$page->Rows = ($page->TotalPages > $page->CurrentPage)
				? $this->GetEmptyArray($pagesize)
				: Array();

			// transfer all of the results into the page object
			$i = 0;
			while ( $obj = $this->Next() )
			{
				$page->Rows[$i++] = $obj;
			}
			
			if (!$countrecords) 
			{
				// we don't know the total count so just set it to the total number of rows in this page
				$page->TotalResults = $i;
			}

			$this->_phreezer->SetValueCache($cachekey, $page, $this->_cache_timeout);

			$this->Clear();

			$this->UnlockCache($cachekey);

		}

		return $page;
	}


    /**
     *
     * @param string $cachekey
     */
    private function GetDelayedCache($cachekey)
    {
    	// if no cache then don't return anything
    	if ($this->_cache_timeout == 0) return null;

        $obj = $this->_phreezer->GetValueCache($cachekey);

    	// no cache, so try three times with a delay to prevent a cache stampede
    	$counter = 1;
		while ( $counter < 4 && $obj == null && $this->IsLocked($cachekey) )
		{
			$this->_phreezer->Observe("(DataSet.GetDelayedCache: flood prevention. delayed attempt ".$counter." of 3...) " . $cachekey,OBSERVE_DEBUG);
			usleep(50000); // 5/100th of a second
			$obj = $this->_phreezer->GetValueCache($cachekey);
			$counter++;
		}

		return $obj;
    }

    /**
     *
     * @param $cachekey
     */
    private function IsLocked($cachekey)
    {
		return $this->_phreezer->LockFilePath && file_exists($this->_phreezer->LockFilePath . md5($cachekey) . ".lock" );
    }

    /**
     *
     * @param $cachekey
     */
    private function LockCache($cachekey)
    {
		if ($this->_phreezer->LockFilePath)
		{
			touch($this->_phreezer->LockFilePath . md5($cachekey) . ".lock");
		}

    }

    /**
     *
     * @param $cachekey
     */
    private function UnlockCache($cachekey)
    {
		if ($this->_phreezer->LockFilePath)
		{
			$lockfile = $this->_phreezer->LockFilePath . md5($cachekey) . ".lock";
			if (file_exists($lockfile)) @unlink($lockfile);
		}
	}

}

?>