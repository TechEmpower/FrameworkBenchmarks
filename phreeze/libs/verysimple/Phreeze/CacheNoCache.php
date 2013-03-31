<?php
/** @package    verysimple::Phreeze */

/** import supporting libraries */
require_once("ICache.php");

/**
 * CacheRam is an implementation of a Cache that doesn't actually cache at all
 *
 * @package    verysimple::Phreeze 
 * @author     VerySimple Inc.
 * @copyright  1997-2008 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    2.0
 */
class CacheNoCache implements ICache
{
	private $ram = array();
	
	public function Get($key,$flags=null)
	{
		return null;
	}
	
	public function Set($key,$val,$flags=null,$timeout=null)
	{
	}

	public function Delete($key)
	{
	}
	
}

?>