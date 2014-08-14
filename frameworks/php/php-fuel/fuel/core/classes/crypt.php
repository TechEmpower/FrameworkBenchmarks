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

use \PHPSecLib\Crypt_AES;
use \PHPSecLib\Crypt_Hash;

class Crypt
{

	/*
	 * Crypto object used to encrypt/decrypt
	 *
	 * @var	object
	 */
	private static $crypter = null;

	/*
	 * Hash object used to generate hashes
	 *
	 * @var	object
	 */
	private static $hasher = null;

	/*
	 * Crypto configuration
	 *
	 * @var	array
	 */
	private static $config = array();

	/*
	 * initialisation and auto configuration
	 */
	public static function _init()
	{
		static::$crypter = new Crypt_AES();
		static::$hasher = new Crypt_Hash('sha256');

		// load the config
		\Config::load('crypt', true);
		static::$config = \Config::get('crypt', array ());

		// generate random crypto keys if we don't have them or they are incorrect length
		$update = false;
		foreach(array('crypto_key', 'crypto_iv', 'crypto_hmac') as $key)
		{
			if ( empty(static::$config[$key]) or (strlen(static::$config[$key]) % 4) != 0)
			{
				$crypto = '';
				for ($i = 0; $i < 8; $i++)
				{
					$crypto .= static::safe_b64encode(pack('n', mt_rand(0, 0xFFFF)));
				}
				static::$config[$key] = $crypto;
				$update = true;
			}
		}

		// update the config if needed
		if ($update === true)
		{
			try
			{
				\Config::save('crypt', static::$config);
			}
			catch (\FileAccessException $e)
			{
				// failed to write the config file, inform the user
				echo \View::forge('errors/crypt_keys', array(
					'keys' => static::$config
				));
				die();
			}
		}

		static::$crypter->enableContinuousBuffer();

		static::$hasher->setKey(static::safe_b64decode(static::$config['crypto_hmac']));
	}

	// --------------------------------------------------------------------

	/*
	 * encrypt a string value, optionally with a custom key
	 *
	 * @param	string	value to encrypt
	 * @param	string	optional custom key to be used for this encryption
	 * @access	public
	 * @return	string	encrypted value
	 */
	public static function encode($value, $key = false)
	{
		$key ? static::$crypter->setKey($key) : static::$crypter->setKey(static::safe_b64decode(static::$config['crypto_key']));
		static::$crypter->setIV(static::safe_b64decode(static::$config['crypto_iv']));

		$value = static::$crypter->encrypt($value);
		return static::safe_b64encode(static::add_hmac($value));

	}

	// --------------------------------------------------------------------

	/*
	 * decrypt a string value, optionally with a custom key
	 *
	 * @param	string	value to decrypt
	 * @param	string	optional custom key to be used for this encryption
	 * @access	public
	 * @return	string	encrypted value
	 */
	public static function decode($value, $key = false)
	{
		$key ? static::$crypter->setKey($key) : static::$crypter->setKey(static::safe_b64decode(static::$config['crypto_key']));
		static::$crypter->setIV(static::safe_b64decode(static::$config['crypto_iv']));

		$value = static::safe_b64decode($value);
		if ($value = static::validate_hmac($value))
		{
			return static::$crypter->decrypt($value);
		}
		else
		{
			return false;
		}
	}

	// --------------------------------------------------------------------

	private static function safe_b64encode($value)
	{
		$data = base64_encode($value);
		$data = str_replace(array('+','/','='), array('-','_',''), $data);
		return $data;
	}

	private static function safe_b64decode($value)
	{
		$data = str_replace(array('-','_'), array('+','/'), $value);
		$mod4 = strlen($data) % 4;
		if ($mod4)
		{
			$data .= substr('====', $mod4);
		}
		return base64_decode($data);
	}

	private static function add_hmac($value)
	{
		// calculate the hmac-sha256 hash of this value
		$hmac = static::safe_b64encode(static::$hasher->hash($value));

		// append it and return the hmac protected string
		return $value.$hmac;
	}

	private static function validate_hmac($value)
	{
		// strip the hmac-sha256 hash from the value
		$hmac = substr($value, strlen($value)-43);

		// and remove it from the value
		$value = substr($value, 0, strlen($value)-43);

		// only return the value if it wasn't tampered with
		return (static::secure_compare(static::safe_b64encode(static::$hasher->hash($value)), $hmac)) ? $value : false;
	}

	private static function secure_compare($a, $b)
	{
		// make sure we're only comparing equal length strings
		if (strlen($a) !== strlen($b))
		{
			return false;
		}

		// and that all comparisons take equal time
		$result = 0;
		for ($i = 0; $i < strlen($a); $i++)
		{
			$result |= ord($a[$i]) ^ ord($b[$i]);
		}
		return $result == 0;
	}
}


