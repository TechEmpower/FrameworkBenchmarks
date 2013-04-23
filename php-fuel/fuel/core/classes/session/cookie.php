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



// --------------------------------------------------------------------

class Session_Cookie extends \Session_Driver
{

	/**
	 * array of driver config defaults
	 */
	protected static $_defaults = array(
		'cookie_name'  => 'fuelcid',
	);

	// --------------------------------------------------------------------

	public function __construct($config = array())
	{
		// merge the driver config with the global config
		$this->config = array_merge($config, (isset($config['cookie']) and is_array($config['cookie'])) ? $config['cookie'] : static::$_defaults);

		$this->config = $this->_validate_config($this->config);
	}

	// --------------------------------------------------------------------

	/**
	 * create a new session
	 *
	 * @access	public
	 * @return	Fuel\Core\Session_Cookie
	 */
	public function create()
	{
		// create a new session
		$this->keys['session_id']	= $this->_new_session_id();
		$this->keys['ip_hash']		= md5(\Input::ip().\Input::real_ip());
		$this->keys['user_agent']	= \Input::user_agent();
		$this->keys['created'] 		= $this->time->get_timestamp();
		$this->keys['updated'] 		= $this->keys['created'];
		$this->keys['payload'] 		= '';

		return $this;
	}

	// --------------------------------------------------------------------

	/**
	 * read the session
	 *
	 * @access	public
	 * @param	boolean, set to true if we want to force a new session to be created
	 * @return	Fuel\Core\Session_Driver
	 */
	public function read($force = false)
	{
		// initialize the session
		$this->data = array();
		$this->keys = array();
		$this->flash = array();

		// get the session cookie
		$payload = $this->_get_cookie();

		// validate it
		if ($payload === false or $force )
		{
			// not a valid cookie, or a forced session reset
		}
		elseif ( ! isset($payload[0]) or ! is_array($payload[0]))
		{
			// not a valid cookie payload
		}
		elseif ($payload[0]['updated'] + $this->config['expiration_time'] <= $this->time->get_timestamp())
		{
			// session has expired
		}
		elseif ($this->config['match_ip'] and $payload[0]['ip_hash'] !== md5(\Input::ip().\Input::real_ip()))
		{
			// IP address doesn't match
		}
		elseif ($this->config['match_ua'] and $payload[0]['user_agent'] !== \Input::user_agent())
		{
			// user agent doesn't match
		}
		else
		{
			// session is valid, retrieve the payload
			if (isset($payload[0]) and is_array($payload[0])) $this->keys  = $payload[0];
			if (isset($payload[1]) and is_array($payload[1])) $this->data  = $payload[1];
			if (isset($payload[2]) and is_array($payload[2])) $this->flash = $payload[2];
		}

		return parent::read();
	}

	// --------------------------------------------------------------------

	/**
	 * write the current session
	 *
	 * @access	public
	 * @return	Fuel\Core\Session_Cookie
	 */
	public function write()
	{
		// do we have something to write?
		if ( ! empty($this->keys) or ! empty($this->data) or ! empty($this->flash))
		{
			parent::write();

			// rotate the session id if needed
			$this->rotate(false);

			// record the last update time of the session
			$this->keys['updated'] = $this->time->get_timestamp();

			// then update the cookie
			$this->_set_cookie(array($this->keys, $this->data, $this->flash));
		}

		return $this;
	}

	// --------------------------------------------------------------------

	/**
	 * validate a driver config value
	 *
	 * @param	array	array with configuration values
	 * @access	public
	 * @return  array	validated and consolidated config
	 */
	public function _validate_config($config)
	{
		$validated = array();

		foreach ($config as $name => $item)
		{
			// filter out any driver config
			if (!is_array($item))
			{
				switch ($name)
				{
					case 'cookie_name':
						if ( empty($item) or ! is_string($item))
						{
							$item = 'fuelcid';
						}
					break;

					default:
						// no config item for this driver
					break;
				}

				// global config, was validated in the driver
				$validated[$name] = $item;
			}
		}

		// validate all global settings as well
		return parent::_validate_config($validated);
	}
}


