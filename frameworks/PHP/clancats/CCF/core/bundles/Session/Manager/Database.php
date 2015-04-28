<?php namespace Session;
/**
 * Session Database Driver
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class Manager_Database implements Manager_Interface
{
	/**
	 * The database instance used to store the sessions
	 *
	 * @var string
	 */
	private $database = null;

	/**
	 * The database table used
	 *
	 * @var string
	 */
	private $table = null;
	
	/**
	 * An array of fields that get used as columns
	 *
	 * @var string
	 */
	private $index_fields = null;
	
	/**
	 * The ID the session started with
	 *
	 * @var string
	 */
	private $inital_sesssion_id = null;

	/**
	 * Cookie driver constructor
	 *
	 * @param string		$name
	 * @param array 		$conf
	 */
	public function __construct( $name, $conf )
	{
		$this->database = \CCArr::get( 'database', $conf );
		$this->table = \CCArr::get( 'table', $conf, 'sessions' );
		$this->index_fields = array_merge( \CCArr::get( 'index_fields', $conf, array() ), array_keys( Manager::default_data_provider() ) );
	}

	/**
	 * Read data from the session dirver
	 *
	 * @param string		$id		The session id key.
	 * @return array
	 */
	public function read( $id )
	{
		$this->inital_session_id = $id;
		
		if ( $data = \DB::select( $this->table )->where( 'id', $id )->one( $this->database ) )
		{
			$session_data = unserialize( $data->content );
			
			foreach( $this->index_fields as $field ) 
			{
				$session_data[$field] = $data->$field;
			}
			
			return $session_data;
		}
		
		return array();
	}

	/**
	 * Check if a session with the given key already exists
	 *
	 * @param string		$id		The session id key.
	 * @return boool
	 */
	public function has( $id )
	{
		return \DB::select( $this->table )
			->where( 'id', $id )
			->column( 'id', $this->database ) == $id;
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
		$columns = array();
		
		foreach( $this->index_fields as $field ) 
		{
			$columns[$field] = $data[$field]; unset( $data[$field] );
		}
		
		$columns['content'] = serialize( $data );
		
		// When the session id didnt change we will do an update
		if ( $id == $this->inital_session_id )
		{
			// because the we might already be in the shutdown process we 
			// have to forward sql error directly to CCError
			try {
				\DB::update( $this->table, $columns )
					->where( 'id', $id )
					->run( $this->database );
			} catch ( \Exception $e ) 
			{
				\CCError::exception( $e );
			}
		}
		else
		// else insert a new one
		{
			// add the id to the columns
			$columns['id'] = $id;
			
			// because the we might already be in the shutdown process we 
			// have to forward sql error directly to CCError
			try {
				\DB::insert( $this->table, $columns )->run( $this->database );
			} catch ( \Exception $e ) 
			{
				\CCError::exception( $e );
			}	
		}
	}

	/**
	 * Delete session that are older than the given time in secounds
	 *
	 * @param int		$time
	 * @return void
	 */
	public function gc( $time )
	{
		// if we don't have the last active key we cannot execute 
		// the garbage collection
		if ( !in_array( 'last_active', $this->index_fields ) )
		{
			return false;
		}
		
		\DB::delete( $this->table )
			->where( 'last_active', '<', time() - $time )
			->run( $this->database );
	}
}