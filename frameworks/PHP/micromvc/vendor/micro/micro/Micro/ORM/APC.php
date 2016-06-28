<?php
/**
 * APC ORM
 *
 * Provides ORM result caching using APC.
 *
 * @package		MicroMVC
 * @author		David Pennington
 * @copyright	(c) 2011 MicroMVC Framework
 * @license		http://micromvc.com/license
 ********************************** 80 Columns *********************************
 */
namespace Micro\ORM;

class APC extends \Micro\ORM
{

	public static function cache_set($key, $value)
	{
		apc_store($key, $value, static::$cache);
	}


	public static function cache_get($key)
	{
		return apc_fetch($key);
	}


	public static function cache_delete($key)
	{
		return apc_delete($key);
	}


	public static function cache_exists($key)
	{
		return apc_exists($key);
	}

}

// END
