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

class Session_Memcached extends \Session_Driver
{

	/**
	 * array of driver config defaults
	 */
	protected static $_defaults = array(
		'cookie_name'		=> 'fuelmid',				// name of the session cookie for memcached based sessions
		'servers'			=> array(					// array of servers and portnumbers that run the memcached service
								array('host' => '127.0.0.1', 'port' => 11211, 'weight' => 100)
							)
	);

	/*
	 * @var	storage for the memcached object
	 */
	protected $memcached = false;

	// --------------------------------------------------------------------

	public function __construct($config = array())
	{
		// merge the driver config with the global config
		$this->config = array_merge($config, is_array($config['memcached']) ? $config['memcached'] : static::$_defaults);

		$this->config = $this->_validate_config($this->config);

		// adjust the expiration time to the maximum possible for memcached
		$this->config['expiration_time'] = min($this->config['expiration_time'], 2592000);
	}

	// --------------------------------------------------------------------

	/**
	 * driver initialisation
	 *
	 * @access	public
	 * @return	void
	 */
	public function init()
	{
		// generic driver initialisation
		parent::init();

		if ($this->memcached === false)
		{
			// do we have the PHP memcached extension available
			if ( ! class_exists('Memcached') )
			{
				throw new \FuelException('Memcached sessions are configured, but your PHP installation doesn\'t have the Memcached extension loaded.');
			}

			// instantiate the memcached object
			$this->memcached = new \Memcached();

			// add the configured servers
			$this->memcached->addServers($this->config['servers']);

			// check if we can connect to the server(s)
			if ($this->memcached->getVersion() === false)
			{
				throw new \FuelException('Memcached sessions are configured, but there is no connection possible. Check your configuration.');
			}
		}
	}

	// --------------------------------------------------------------------

	/**
	 * create a new session
	 *
	 * @access	public
	 * @return	Fuel\Core\Session_Memcached
	 */
	public function create()
	{
		// create a new session
		$this->keys['session_id']	= $this->_new_session_id();
		$this->keys['previous_id']	= $this->keys['session_id'];	// prevents errors if previous_id has a unique index
		$this->keys['ip_hash']		= md5(\Input::ip().\Input::real_ip());
		$this->keys['user_agent']	= \Input::user_agent();
		$this->keys['created'] 		= $this->time->get_timestamp();
		$this->keys['updated'] 		= $this->keys['created'];

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
		$cookie = $this->_get_cookie();

		// if a cookie was present, find the session record
		if ($cookie and ! $force and isset($cookie[0]))
		{
			// read the session file
			$payload = $this->_read_memcached($cookie[0]);

			if ($payload === false)
			{
				// cookie present, but session record missing. force creation of a new session
				return $this->read(true);
			}

			// unpack the payload
			$payload = $this->_unserialize($payload);

			// session referral?
			if (isset($payload['rotated_session_id']))
			{
				$payload = $this->_read_memcached($payload['rotated_session_id']);
				if ($payload === false)
				{
					// cookie present, but session record missing. force creation of a new session
					return $this->read(true);
				}
				else
				{
					// unpack the payload
					$payload = $this->_unserialize($payload);
				}
			}

			if ( ! isset($payload[0]) or ! is_array($payload[0]))
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
				// session is valid, retrieve the rest of the payload
				if (isset($payload[0]) and is_array($payload[0])) $this->keys  = $payload[0];
				if (isset($payload[1]) and is_array($payload[1])) $this->data  = $payload[1];
				if (isset($payload[2]) and is_array($payload[2])) $this->flash = $payload[2];
			}
		}

		return parent::read();
	}

	// --------------------------------------------------------------------

	/**
	 * write the session
	 *
	 * @access	public
	 * @return	Fuel\Core\Session_Memcached
	 */
	public function write()
	{
		// do we have something to write?
		if ( ! empty($this->keys) or ! empty($this->data) or ! empty($this->flash))
		{
			parent::write();

			// rotate the session id if needed
			$this->rotate(false);

			// session payload
			$payload = $this->_serialize(array($this->keys, $this->data, $this->flash));

			// create the session file
			$this->_write_memcached($this->keys['session_id'], $payload);

			// was the session id rotated?
			if ( isset($this->keys['previous_id']) and $this->keys['previous_id'] != $this->keys['session_id'])
			{
				// point the old session file to the new one, we don't want to lose the session
				$payload = $this->_serialize(array('rotated_session_id' => $this->keys['session_id']));
				$this->_write_memcached($this->keys['previous_id'], $payload);
			}

			$this->_set_cookie(array($this->keys['session_id']));
		}

		return $this;
	}

	// --------------------------------------------------------------------

	/**
	 * destroy the current session
	 *
	 * @access	public
	 * @return	Fuel\Core\Session_Memcached
	 */
	public function destroy()
	{
		// do we have something to destroy?
		if ( ! empty($this->keys))
		{
			// delete the key from the memcached server
			if ($this->memcached->delete($this->config['cookie_name'].'_'.$this->keys['session_id']) === false)
			{
				throw new \FuelException('Memcached returned error code "'.$this->memcached->getResultCode().'" on delete. Check your configuration.');
			}
		}

		parent::destroy();

		return $this;
	}

	// --------------------------------------------------------------------

	/**
	 * Writes the memcached entry
	 *
	 * @access	private
	 * @return  boolean, true if it was an existing session, false if not
	 */
	protected function _write_memcached($session_id, $payload)
	{
		// write it to the memcached server
		if ($this->memcached->set($this->config['cookie_name'].'_'.$session_id, $payload, $this->config['expiration_time']) === false)
		{
			throw new \FuelException('Memcached returned error code "'.$this->memcached->getResultCode().'" on write. Check your configuration.');
		}
	}

	// --------------------------------------------------------------------

	/**
	 * Reads the memcached entry
	 *
	 * @access	private
	 * @return  mixed, the payload if the file exists, or false if not
	 */
	protected function _read_memcached($session_id)
	{
		// fetch the session data from the Memcached server
		return $this->memcached->get($this->config['cookie_name'].'_'.$session_id);
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
			if ($name == 'memcached' and is_array($item))
			{
				foreach ($item as $name => $value)
				{
					switch ($name)
					{
						case 'cookie_name':
							if ( empty($value) or ! is_string($value))
							{
								$value = 'fuelmid';
							}
						break;

						case 'servers':
							// do we have a servers config
							if ( empty($value) or ! is_array($value))
							{
								$value = array('default' => array('host' => '127.0.0.1', 'port' => '11211'));
							}

							// validate the servers
							foreach ($value as $key => $server)
							{
								// do we have a host?
								if ( ! isset($server['host']) or ! is_string($server['host']))
								{
									throw new \FuelException('Invalid Memcached server definition in the session configuration.');
								}
								// do we have a port number?
								if ( ! isset($server['port']) or ! is_numeric($server['port']) or $server['port'] < 1025 or $server['port'] > 65535)
								{
									throw new \FuelException('Invalid Memcached server definition in the session configuration.');
								}
								// do we have a relative server weight?
								if ( ! isset($server['weight']) or ! is_numeric($server['weight']) or $server['weight'] < 0)
								{
									// set a default
									$value[$key]['weight'] = 0;
								}
							}
						break;

						default:
							// unknown property
							continue;
					}

					$validated[$name] = $value;
				}
			}
			else
			{
				// skip all config array properties
				if (is_array($item))
				{
					continue;
				}

				// global config, was validated in the driver
				$validated[$name] = $item;
			}

		}

		// validate all global settings as well
		return parent::_validate_config($validated);
	}

}


