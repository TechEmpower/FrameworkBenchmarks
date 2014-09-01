<?php namespace Session;
/**
 * Session Array test driver
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class Manager_Array implements Manager_Interface
{
	/**
	 * the data holder
	 *
	 * @var array
	 */
	private $data = array();
	
	/**
	 * Read data from the session dirver
	 *
	 * @param string		$id		The session id key.
	 * @return array
	 */
	public function read( $id )
	{
		if ( array_key_exists( $id, $this->data ) )
		{
			return $this->data[$id];
		}
	}
	
	/**
	 * Check if a session with the given key already exists
	 *
	 * @param string		$id		The session id key.
	 * @return bool
	 */
	public function has( $id )
	{
		return array_key_exists( $id, $this->data );
	}
	
	/**
	 * Check if a session with the given key already exists
	 *
	 * @param string		$id		The session id key.
	 * @param array 		$data
	 * @return bool
	 */
	public function write( $id, $data )
	{
		$this->data[$id] = $data;
	}
	
	/**
	 * Delete session that are older than the given time in secounds
	 *
	 * @param int		$time
	 * @return void
	 */
	public function gc( $time )
	{
		$this->data = array();
	}
}