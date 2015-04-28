<?php namespace DB;
/**
 * Handler wraps PDO and handles the final queries
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class Handler_Driver
{
	/**
	 * the raw connection object
	 *
	 * @var \PDO
	 */
	protected $connection = null;
	
	/**
	 * the string used for the PDO connection
	 *
	 * @var string
	 */
	protected $connection_string = null;
	
	/**
	 * connect to database
	 *
	 * @param array 		$conf
	 * @return bool
	 */
	public function connect( $conf ) 
	{	
		$connection_params = array();
		
		foreach( $conf as $key => $value )
		 {
			if ( is_string( $value ) ) 
			{
				$connection_params[ '{'.$key.'}' ] = $value;
			}
		}
		
		$connection_string = \CCStr::replace( $this->connection_string, $connection_params );
		
		$this->connection = new \PDO( $connection_string, $conf['user'], $conf['pass'], $this->connection_attributes( $conf ) );
		
		// At the moment this will never happen because pdo is going to 
		// trhow an exception when the connection fails
		/*if ( !$this->connection )
		{
			return false;
		}*/
		
		// let pdo throw exceptions
		$this->connection->setAttribute(\PDO::ATTR_ERRMODE, \PDO::ERRMODE_EXCEPTION);
		
		return true;
	}
	
	/**
	 * Get the current PDO conncection object
	 *
	 * @return \PDO
	 */
	public function connection()
	{
		return $this->connection;
	}
	
	/**
	 * return the connection attributes
	 *
	 * @param array 		$conf
	 * @return array
	 */
	protected function connection_attributes( $conf ) 
	{
		return array();
	}
}