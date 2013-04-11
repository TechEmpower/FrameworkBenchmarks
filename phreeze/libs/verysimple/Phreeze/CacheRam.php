<?php
/** @package    verysimple::Phreeze */

/** import supporting libraries */
require_once("ICache.php");

/**
 * CacheRam is an implementation of a Cache that persists to ram for the current page load only
 *
 * @package    verysimple::Phreeze 
 * @author     VerySimple Inc.
 * @copyright  1997-2008 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    2.0
 */
class CacheRam implements ICache
{
	private $ram = array();
	
	public function Get($key,$flags=null)
	{
		return isset($this->ram[$key]) ? $this->ram[$key] : null;
	}
	
	public function GetKeys()
	{
		return array_keys($this->ram);
	}
	
	public function Set($key,$val,$flags=null,$timeout=null)
	{
		$this->ram[$key] = $val;
	}

	public function Delete($key)
	{
		if (isset($this->ram[$key])) unset($this->ram[$key]);
	}
	
}

?>