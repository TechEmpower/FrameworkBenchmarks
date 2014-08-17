<?php defined('SYSPATH') OR die('No direct script access.');
/**
 * Database-based session class.
 *
 * Sample schema:
 *
 *     CREATE TABLE  `sessions` (
 *         `session_id` VARCHAR( 24 ) NOT NULL,
 *         `last_active` INT UNSIGNED NOT NULL,
 *         `contents` TEXT NOT NULL,
 *         PRIMARY KEY ( `session_id` ),
 *         INDEX ( `last_active` )
 *     ) ENGINE = MYISAM ;
 *
 * @package    Kohana/Database
 * @category   Session
 * @author     Kohana Team
 * @copyright  (c) 2008-2009 Kohana Team
 * @license    http://kohanaphp.com/license
 */
class Kohana_Session_Database extends Session {

	// Database instance
	protected $_db;

	// Database table name
	protected $_table = 'sessions';

	// Database column names
	protected $_columns = array(
		'session_id'  => 'session_id',
		'last_active' => 'last_active',
		'contents'    => 'contents'
	);

	// Garbage collection requests
	protected $_gc = 500;

	// The current session id
	protected $_session_id;

	// The old session id
	protected $_update_id;

	public function __construct(array $config = NULL, $id = NULL)
	{
		if ( ! isset($config['group']))
		{
			// Use the default group
			$config['group'] = Database::$default;
		}

		// Load the database
		$this->_db = Database::instance($config['group']);

		if (isset($config['table']))
		{
			// Set the table name
			$this->_table = (string) $config['table'];
		}

		if (isset($config['gc']))
		{
			// Set the gc chance
			$this->_gc = (int) $config['gc'];
		}

		if (isset($config['columns']))
		{
			// Overload column names
			$this->_columns = $config['columns'];
		}

		parent::__construct($config, $id);

		if (mt_rand(0, $this->_gc) === $this->_gc)
		{
			// Run garbage collection
			// This will average out to run once every X requests
			$this->_gc();
		}
	}

	public function id()
	{
		return $this->_session_id;
	}

	protected function _read($id = NULL)
	{
		if ($id OR $id = Cookie::get($this->_name))
		{
			$result = DB::select(array($this->_columns['contents'], 'contents'))
				->from($this->_table)
				->where($this->_columns['session_id'], '=', ':id')
				->limit(1)
				->param(':id', $id)
				->execute($this->_db);

			if ($result->count())
			{
				// Set the current session id
				$this->_session_id = $this->_update_id = $id;

				// Return the contents
				return $result->get('contents');
			}
		}

		// Create a new session id
		$this->_regenerate();

		return NULL;
	}

	protected function _regenerate()
	{
		// Create the query to find an ID
		$query = DB::select($this->_columns['session_id'])
			->from($this->_table)
			->where($this->_columns['session_id'], '=', ':id')
			->limit(1)
			->bind(':id', $id);

		do
		{
			// Create a new session id
			$id = str_replace('.', '-', uniqid(NULL, TRUE));

			// Get the the id from the database
			$result = $query->execute($this->_db);
		}
		while ($result->count());

		return $this->_session_id = $id;
	}

	protected function _write()
	{
		if ($this->_update_id === NULL)
		{
			// Insert a new row
			$query = DB::insert($this->_table, $this->_columns)
				->values(array(':new_id', ':active', ':contents'));
		}
		else
		{
			// Update the row
			$query = DB::update($this->_table)
				->value($this->_columns['last_active'], ':active')
				->value($this->_columns['contents'], ':contents')
				->where($this->_columns['session_id'], '=', ':old_id');

			if ($this->_update_id !== $this->_session_id)
			{
				// Also update the session id
				$query->value($this->_columns['session_id'], ':new_id');
			}
		}

		$query
			->param(':new_id',   $this->_session_id)
			->param(':old_id',   $this->_update_id)
			->param(':active',   $this->_data['last_active'])
			->param(':contents', $this->__toString());

		// Execute the query
		$query->execute($this->_db);

		// The update and the session id are now the same
		$this->_update_id = $this->_session_id;

		// Update the cookie with the new session id
		Cookie::set($this->_name, $this->_session_id, $this->_lifetime);

		return TRUE;
	}

	/**
	 * @return  bool
	 */
	protected function _restart()
	{
		$this->_regenerate();

		return TRUE;
	}

	protected function _destroy()
	{
		if ($this->_update_id === NULL)
		{
			// Session has not been created yet
			return TRUE;
		}

		// Delete the current session
		$query = DB::delete($this->_table)
			->where($this->_columns['session_id'], '=', ':id')
			->param(':id', $this->_update_id);

		try
		{
			// Execute the query
			$query->execute($this->_db);

			// Delete the cookie
			Cookie::delete($this->_name);
		}
		catch (Exception $e)
		{
			// An error occurred, the session has not been deleted
			return FALSE;
		}

		return TRUE;
	}

	protected function _gc()
	{
		if ($this->_lifetime)
		{
			// Expire sessions when their lifetime is up
			$expires = $this->_lifetime;
		}
		else
		{
			// Expire sessions after one month
			$expires = Date::MONTH;
		}

		// Delete all sessions that have expired
		DB::delete($this->_table)
			->where($this->_columns['last_active'], '<', ':time')
			->param(':time', time() - $expires)
			->execute($this->_db);
	}

} // End Session_Database
