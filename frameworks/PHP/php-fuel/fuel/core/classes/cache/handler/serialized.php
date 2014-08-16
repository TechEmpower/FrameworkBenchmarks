<?php
/**
 * Part of the Fuel framework.
 *
 * @package    Fuel
 * @version    1.5
 * @author     Fuel Development Team
 * @license    MIT License
 * @copyright  2010 - 2013 Fuel Development Team
 * @link       http://fuelphp.com
 */

namespace Fuel\Core;



class Cache_Handler_Serialized implements \Cache_Handler_Driver
{

	public function readable($contents)
	{
		return unserialize($contents);
	}

	public function writable($contents)
	{
		return serialize($contents);
	}

}


