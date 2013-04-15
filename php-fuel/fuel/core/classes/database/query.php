<?php
/**
 * Database query wrapper.
 *
 * @package    Fuel/Database
 * @category   Query
 * @author     Kohana Team
 * @copyright  (c) 2008-2009 Kohana Team
 * @license    http://kohanaphp.com/license
 */

namespace Fuel\Core;



class Database_Query
{

	/**
	 * @var  int  Query type
	 */
	protected $_type;

	/**
	 * @var  int  Cache lifetime
	 */
	protected $_lifetime;

	/**
	 * @var  string  Cache key
	 */
	protected $_cache_key = null;

	/**
	 * @var  boolean  Cache all results
	 */
	protected $_cache_all = true;

	/**
	 * @var  string  SQL statement
	 */
	protected $_sql;

	/**
	 * @var  array  Quoted query parameters
	 */
	protected $_parameters = array();

	/**
	 * @var  bool  Return results as associative arrays or objects
	 */
	protected $_as_object = false;

	/**
	 * Creates a new SQL query of the specified type.
	 *
	 * @param   string   query string
	 * @param   integer  query type: DB::SELECT, DB::INSERT, etc
	 * @return  void
	 */
	public function __construct($sql, $type = null)
	{
		$this->_type = $type;
		$this->_sql = $sql;
	}

	/**
	 * Return the SQL query string.
	 *
	 * @return  string
	 */
	final public function __toString()
	{
		try
		{
			// Return the SQL string
			return $this->compile(\Database_Connection::instance());
		}
		catch (\Exception $e)
		{
			return $e->getMessage();
		}
	}

	/**
	 * Get the type of the query.
	 *
	 * @return  integer
	 */
	public function type()
	{
		return $this->_type;
	}

	/**
	 * Enables the query to be cached for a specified amount of time.
	 *
	 * @param   integer  number of seconds to cache or null for default
	 * @param   string   name of the cache key to be used or null for default
	 * @param   boolean  if true, cache all results, even empty ones
	 * @return  $this
	 */
	public function cached($lifetime = null, $cache_key = null, $cache_all = true)
	{
		$this->_lifetime = $lifetime;
		$this->_cache_all = (bool) $cache_all;
		is_string($cache_key) and $this->_cache_key = $cache_key;

		return $this;
	}

	/**
	 * Returns results as associative arrays
	 *
	 * @return  $this
	 */
	public function as_assoc()
	{
		$this->_as_object = false;

		return $this;
	}

	/**
	 * Returns results as objects
	 *
	 * @param   string  classname or true for stdClass
	 * @return  $this
	 */
	public function as_object($class = true)
	{
		$this->_as_object = $class;

		return $this;
	}

	/**
	 * Set the value of a parameter in the query.
	 *
	 * @param   string   parameter key to replace
	 * @param   mixed    value to use
	 * @return  $this
	 */
	public function param($param, $value)
	{
		// Add or overload a new parameter
		$this->_parameters[$param] = $value;

		return $this;
	}

	/**
	 * Bind a variable to a parameter in the query.
	 *
	 * @param   string  parameter key to replace
	 * @param   mixed   variable to use
	 * @return  $this
	 */
	public function bind($param, & $var)
	{
		// Bind a value to a variable
		$this->_parameters[$param] =& $var;

		return $this;
	}

	/**
	 * Add multiple parameters to the query.
	 *
	 * @param   array  list of parameters
	 * @return  $this
	 */
	public function parameters(array $params)
	{
		// Merge the new parameters in
		$this->_parameters = $params + $this->_parameters;

		return $this;
	}

	/**
	 * Compile the SQL query and return it. Replaces any parameters with their
	 * given values.
	 *
	 * @param   mixed  Database instance or instance name
	 * @return  string
	 */
	public function compile($db = null)
	{
		if ( ! $db instanceof \Database_Connection)
		{
			// Get the database instance
			$db = \Database_Connection::instance($db);
		}

		// Import the SQL locally
		$sql = $this->_sql;

		if ( ! empty($this->_parameters))
		{
			// Quote all of the values
			$values = array_map(array($db, 'quote'), $this->_parameters);

			// Replace the values in the SQL
			$sql = \Str::tr($sql, $values);
		}

		return trim($sql);
	}

	/**
	 * Execute the current query on the given database.
	 *
	 * @param   mixed    Database instance or name of instance
	 * @return  object   Database_Result for SELECT queries
	 * @return  mixed    the insert id for INSERT queries
	 * @return  integer  number of affected rows for all other queries
	 */
	public function execute($db = null)
	{
		if ( ! is_object($db))
		{
			// Get the database instance
			$db = \Database_Connection::instance($db);
		}

		// Compile the SQL query
		$sql = $this->compile($db);

		switch(strtoupper(substr(ltrim($sql,'('), 0, 6)))
		{
			case 'SELECT':
				$this->_type = \DB::SELECT;
				break;
			case 'INSERT':
			case 'CREATE':
				$this->_type = \DB::INSERT;
				break;
		}

		if ($db->caching() and ! empty($this->_lifetime) and $this->_type === DB::SELECT)
		{
			$cache_key = empty($this->_cache_key) ?
				'db.'.md5('Database_Connection::query("'.$db.'", "'.$sql.'")') : $this->_cache_key;
			$cache = \Cache::forge($cache_key);
			try
			{
				$result = $cache->get();
				return new Database_Result_Cached($result, $sql, $this->_as_object);
			}
			catch (CacheNotFoundException $e) {}
		}

		// Execute the query
		\DB::$query_count++;
		$result = $db->query($this->_type, $sql, $this->_as_object);

		// Cache the result if needed
		if (isset($cache) and ($this->_cache_all or $result->count()))
		{
			$cache->set_expiration($this->_lifetime)->set_contents($result->as_array())->set();
		}

		return $result;
	}

}
