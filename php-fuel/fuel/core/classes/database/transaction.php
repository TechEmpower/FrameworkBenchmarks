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

/**
 * Interact with Database Transactions
 *
 * @author     five07
 * @copyright  (c) 2011 five07
 * @license    https://github.com/five07/Fuel-Addons/blob/master/LICENSE
 */

namespace Fuel\Core;


/**
 * @deprecated  remove in v1.2
 */
class Database_Transaction
{
	/**
	 * @var  Database_Transaction  for Singleton-like usage
	 */
	protected static $_instance = null;

	/**
	 * @var  \Fuel\Core\Database_Connection
	 */
	protected $_db;

	public static function instance()
	{
		if (static::$_instance == null)
		{
			static::$_instance = static::forge();
		}
		return static::$_instance;
	}

	/**
	 * Creates a new instance
	 *
	 * @param  string  $instance
	 */
	public static function forge($instance = null)
	{
		logger(\Fuel::L_WARNING, 'The Database_Transaction class is deprecated, use the connection driver methods instead.', __METHOD__);
		return new static($instance);
	}

	/**
	* The constructor
	 *
	 * @param  string  $instance
	*/
	public function __construct($instance = null)
	{
		$this->_db = Database_Connection::instance($instance);
	}

	/**
	 * Start your transaction before a set of dependent queries
	 */
	public function start()
	{
		$this->_db->start_transaction();
	}

	/**
	 * Complete your transaction on the set of queries
	 */
	public function complete()
	{
		try
		{
			static::commit();
		}
		catch (\Exception $e)
		{
			static::rollback();
		}
	}

	/**
	 * If the group of queries had no errors, this returns TRUE
	 * Otherwise, will return FALSE
	 *
	 * @return  boolean
	 */
	public function status()
	{
		return true;
	}

	/**
	 * Commit the successful queries and reset AUTOCOMMIT
	 * This is called automatically if you use Database_Transaction::complete()
	 * It can also be used manually for testing
	 */
	public function commit()
	{
		$this->_db->commit_transaction();
	}

	/**
	 * Rollback the failed queries and reset AUTOCOMMIT
	 * This is called automatically if you use Database_Transaction::complete()
	 * It can also be used manually for testing
	 */
	public function rollback()
	{
		$this->_db->rollback_transaction();
	}

	/**
	 * Return the database errors
	 *
	 * @return mixed (array or false)
	 */
	public function errors()
	{
		return false;
	}


}
