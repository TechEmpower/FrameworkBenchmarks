<?php
/**
 * Cookie
 *
 * Provides a encryption wrapper around standard cookie handling functions.
 *
 * @package		MicroMVC
 * @author		David Pennington
 * @copyright	(c) 2011 MicroMVC Framework
 * @license		http://micromvc.com/license
 ********************************** 80 Columns *********************************
 */
namespace Micro;

class Cookie
{

	public static $settings = array();

	/**
	 * Decrypt and fetch cookie data
	 *
	 * @param string $name of cookie
	 * @param array $config settings
	 * @return mixed
	 */
	public static function get($name, $config = NULL)
	{
		// Use default config settings if needed
		$config = $config ?: static::$settings;

		if(isset($_COOKIE[$name]))
		{
			// Decrypt cookie using cookie key
			if($v = json_decode(Cipher::decrypt(base64_decode($_COOKIE[$name]), $config['key'])))
			{
				// Has the cookie expired?
				if($v[0] < $config['timeout'])
				{
					return is_scalar($v[1])?$v[1]:(array)$v[1];
				}
			}
		}

		return FALSE;
	}


	/**
	 * Called before any output is sent to create an encrypted cookie with the given value.
	 *
	 * @param string $key cookie name
	 * @param mixed $value to save
	 * @param array $config settings
	 * return boolean
	 */
	public static function set($name, $value, $config = NULL)
	{
		// Use default config settings if needed
		extract($config ?: static::$settings);

		// If the cookie is being removed we want it left blank
		$value = $value ? base64_encode(Cipher::encrypt(json_encode(array(time(), $value)), $key)) : '';

		// Save cookie to user agent
		setcookie($name, $value, $expires, $path, $domain, $secure, $httponly);
	}

}

// END
