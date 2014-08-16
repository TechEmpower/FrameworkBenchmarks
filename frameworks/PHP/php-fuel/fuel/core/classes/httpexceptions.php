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


class HttpNotFoundException extends \HttpException
{
	public function response()
	{
		return new \Response(\View::forge('404'), 404);
	}
}

class HttpServerErrorException extends \HttpException
{
	public function response()
	{
		return new \Response(\View::forge('500'), 500);
	}
}
