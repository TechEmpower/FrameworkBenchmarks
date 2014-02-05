<?php
/** @package    verysimple::Util */

require_once("verysimple/Phreeze/CacheMemCache.php");

/**
* MemCacheProxy provides simple access to memcache pool but ignores
* if the server is down instead of throwing an error.  if the server
* could not be contacted, ServerOffline will be set to true.
* 
* @package    verysimple::Util
* @author     VerySimple Inc.
* @copyright  1997-2007 VerySimple, Inc.
* @license    http://www.gnu.org/licenses/lgpl.html  LGPL
* @version    1.0
*/
class MemCacheProxy extends CacheMemCache
{
	public $ServerOffline = false;
	public $LastServerError = '';

	/**
	 * Acts as a proxy for a MemCache server and fails gracefull if the pool cannot be contacted
	 * @param array in host/port format: array('host1'=>'11211','host2'=>'11211')
	 * @param string a unique string.  prevents conflicts in case multiple apps are using the same memcached server bank
	 */
	public function __construct($server_array = array('localhost'=>'11211'),$uniquePrefix = "CACHE-")
	{
		if (class_exists('Memcache'))
		{
			$memcache = new Memcache();
			foreach (array_keys($server_array) as $host)
			{
				// print "adding server $host " . $server_array[$host];
				$memcache->addServer($host, $server_array[$host]);
			}
			
			parent::__construct($memcache,$uniquePrefix,true);
		}
		else
		{
			$this->LastServerError = 'Memcache client module not installed';
			$this->ServerOffline = true;
		}
	}


	/**
	 * @inheritdocs
	 */
	public function Get($key,$flags=null)
	{
		// prevent hammering the server if it is down
		if ($this->ServerOffline) return null;

		return parent::Get($key,$flags);
	}


	/**
	 * @inheritdocs
	 */
	public function Set($key, $val, $flags = null, $timeout = 0)
	{
		// prevent hammering the server if it is down
		if ($this->ServerOffline) return null;

		return parent::Set($key, $val, $flags, $timeout);
	}

	/**
	 * @inheritdocs
	 */
	public function Delete($key)
	{
		// prevent hammering the server if it is down
		if ($this->ServerOffline) return null;
		
		return parent::Delete($key);
	}
}

?>