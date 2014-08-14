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



class Cache_Handler_Json implements \Cache_Handler_Driver
{

	public function readable($contents)
	{
		$array = false;
		if (substr($contents, 0, 1) == 'a')
		{
			$contents = substr($contents, 1);
			$array = true;
		}

		return json_decode($contents, $array);
	}

	public function writable($contents)
	{
		$array = '';
		if (is_array($contents))
		{
			$array = 'a';
		}

		return $array.json_encode($contents);
	}
}


