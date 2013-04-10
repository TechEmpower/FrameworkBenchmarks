<?php
/** @package    verysimple::Phreeze */

/**
 * ICache defines an interface for a caching mechanism
 *
 * @package    verysimple::Phreeze 
 * @author     VerySimple Inc.
 * @copyright  1997-2007 VerySimple, Inc.
 * @license    http://www.gnu.org/licenses/lgpl.html  LGPL
 * @version    2.0
 */
interface ICache
{
	/**
	* Retreives a value from the cache
	*
	* @access     public
	* @param string $key
	*/
	public function Get($key);
	
	/**
	* Stores a value in the cache
	*
	* @access     public
	* @param string $key
	* @param variant $val
	* @param int $flags
	* @param int $timout in miliseconds
	* @return variant
	*/
	public function Set($key,$val,$flags=null,$timeout=null);

	/**
	* Removes a value from the cache
	*
	* @access     public
	* @param string $key
	*/
	public function Delete($key);
}

?>