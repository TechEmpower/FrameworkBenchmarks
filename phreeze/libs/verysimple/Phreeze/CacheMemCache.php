<?php
/** @package    verysimple::Phreeze */

/** import supporting libraries */
require_once("ICache.php");
require_once("verysimple/Util/ExceptionThrower.php");

/**
 * CacheRam is an implementation of a Cache that persists to ram for the current page load only
 *
 * @package    verysimple::Phreeze 
 * @author     VerySimple Inc.
 * @copyright  1997-2008 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    2.0
 */
class CacheMemCache implements ICache
{
	private $_memcache = null;
	private $_prefix = "";
	private $_suppressServerErrors = false;
	private $_lockFilePath = "";
	
	/**
	 * Constructor requires a reference to a MemCache object
	 * @param Memcache memcache object
	 * @param string a unique prefix to use so this app doesn't conflict with any others that may use the same memcache pool
	 * @param bool set to true to ignore errors if a connection can't be made to the cache server
	 */
	public function __construct($memcache,$uniquePrefix = "CACHE-",$suppressServerErrors=false)
	{
		$this->_memcache = $memcache;
		$this->_prefix = $uniquePrefix ? $uniquePrefix . "-" : "";
		$this->_suppressServerErrors = $suppressServerErrors;
	}
	
	/**
	 * @inheritdocs
	 */
	public function Get($key,$flags=null)
	{
		$obj = null;
		try
		{
			ExceptionThrower::Start();
			$obj = $this->_memcache->get($this->_prefix . $key);
			ExceptionThrower::Stop();
		}
		catch (Exception $ex)
		{
			ExceptionThrower::Stop();
			if (!$this->_suppressServerErrors) throw $ex;
		}

		return $obj;
	}
	
	/**
	 * @inheritdocs
	 */
	public function Set($key,$val,$flags=null,$timeout=null)
	{
		$result = null;
		try
		{
			ExceptionThrower::Start();
			$result = $this->_memcache->set($this->_prefix . $key,$val,$flags,$timeout);
			ExceptionThrower::Stop();
		}
		catch (Exception $ex)
		{
			ExceptionThrower::Stop();
			if (!$this->_suppressServerErrors) throw $ex;
		}
		
		return $result;
	}

	/**
	 * @inheritdocs
	 */
	public function Delete($key)
	{
		$result = null;
		try
		{
			ExceptionThrower::Start();
			$result = $this->_memcache->delete($this->_prefix . $key);
			ExceptionThrower::Stop();
		}
		catch (Exception $ex)
		{
			ExceptionThrower::Stop();
			if (!$this->_suppressServerErrors) throw $ex;
		}
		
		return $result;
	}
	
	}

?>