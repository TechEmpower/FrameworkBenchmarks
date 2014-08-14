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

class Session_Db extends \Session_Driver
{

	/*
	 * @var	session database result object
	 */
	protected $record = null;

	/**
	 * array of driver config defaults
	 */
	protected static $_defaults = array(
		'cookie_name'		=> 'fueldid',				// name of the session cookie for database based sessions
		'table'				=> 'sessions',				// name of the sessions table
		'gc_probability'	=> 5						// probability % (between 0 and 100) for garbage collection
	);

	// --------------------------------------------------------------------

	public function __construct($config = array())
	{
		// merge the driver config with the global config
		$this->config = array_merge($config, is_array($config['db']) ? $config['db'] : static::$_defaults);

		$this->config = $this->_validate_config($this->config);
	}

	// --------------------------------------------------------------------

	/**
	 * create a new session
	 *
	 * @access	public
	 * @return	Fuel\Core\Session_Db
	 */
	public function create($payload = '')
	{
		// create a new session
		$this->keys['session_id']	= $this->_new_session_id();
		$this->keys['previous_id']	= $this->keys['session_id'];	// prevents errors if previous_id has a unique index
		$this->keys['ip_hash']		= md5(\Input::ip().\Input::real_ip());
		$this->keys['user_agent']	= \Input::user_agent();
		$this->keys['created'] 		= $this->time->get_timestamp();
		$this->keys['updated'] 		= $this->keys['created'];

		// add the payload
		$this->keys['payload'] = $payload;

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
		$this->record = null;

		// get the session cookie
		$cookie = $this->_get_cookie();

		// if a cookie was present, find the session record
		if ($cookie and ! $force and isset($cookie[0]))
		{
			// read the session record
			$this->record = \DB::select()->where('session_id', '=', $cookie[0])->from($this->config['table'])->execute($this->config['database']);

			// record found?
			if ($this->record->count())
			{
				$payload = $this->_unserialize($this->record->get('payload'));
			}
			else
			{
				// try to find the session on previous id
				$this->record = \DB::select()->where('previous_id', '=', $cookie[0])->from($this->config['table'])->execute($this->config['database']);

				// record found?
				if ($this->record->count())
				{
					$payload = $this->_unserialize($this->record->get('payload'));
				}
				else
				{
					// cookie present, but session record missing. force creation of a new session
					return $this->read(true);
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
				// session is valid, retrieve the payload
				if (isset($payload[0]) and is_array($payload[0])) $this->keys  = $payload[0];
				if (isset($payload[1]) and is_array($payload[1])) $this->data  = $payload[1];
				if (isset($payload[2]) and is_array($payload[2])) $this->flash = $payload[2];
			}
		}

		return parent::read();
	}

	// --------------------------------------------------------------------

	/**
	 * write the current session
	 *
	 * @access	public
	 * @return	Fuel\Core\Session_Db
	 */
	public function write()
	{
		// do we have something to write?
		if ( ! empty($this->keys) or ! empty($this->data) or ! empty($this->flash))
		{
			parent::write();

			// rotate the session id if needed
			$this->rotate(false);

			// create the session record, and add the session payload
			$session = $this->keys;
			$session['payload'] = $this->_serialize(array($this->keys, $this->data, $this->flash));

			// do we need to create a new session?
			if (is_null($this->record))
			{
				// create the new session record
				$result = \DB::insert($this->config['table'], array_keys($session))->values($session)->execute($this->config['database']);
			}
			else
			{
				// update the database
				$result = \DB::update($this->config['table'])->set($session)->where('session_id', '=', $this->record->get('session_id'))->execute($this->config['database']);
			}

			// update went well?
			if ($result !== false)
			{
				// then update the cookie
				$this->_set_cookie(array($this->keys['session_id']));
			}
			else
			{
				logger(\Fuel::L_ERROR, 'Session update failed, session record could not be found. Concurrency issue?');
			}

			// do some garbage collection
			if (mt_rand(0,100) < $this->config['gc_probability'])
			{
				$expired = $this->time->get_timestamp() - $this->config['expiration_time'];
				$result = \DB::delete($this->config['table'])->where('updated', '<', $expired)->execute($this->config['database']);
			}
		}

		return $this;
	}

	// --------------------------------------------------------------------

	/**
	 * destroy the current session
	 *
	 * @access	public
	 * @return	Fuel\Core\Session_Db
	 */
	public function destroy()
	{
		// do we have something to destroy?
		if ( ! empty($this->keys) and ! empty($this->record))
		{
			// delete the session record
			$result = \DB::delete($this->config['table'])->where('session_id', '=', $this->keys['session_id'])->execute($this->config['database']);
		}

		// reset the stored session data
		$this->record = null;

		parent::destroy();

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
							$item = 'fueldid';
						}
					break;

					case 'database':
						// do we have a database?
						if ( empty($item) or ! is_string($item))
						{
							\Config::load('db', true);
							$item = \Config::get('db.active', false);
						}
						if ($item === false)
						{
							throw new \FuelException('You have specify a database to use database backed sessions.');
						}
					break;

					case 'table':
						// and a table name?
						if ( empty($item) or ! is_string($item))
						{
							throw new \FuelException('You have specify a database table name to use database backed sessions.');
						}
					break;

					case 'gc_probability':
						// do we have a path?
						if ( ! is_numeric($item) or $item < 0 or $item > 100)
						{
							// default value: 5%
							$item = 5;
						}
					break;

					default:
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
