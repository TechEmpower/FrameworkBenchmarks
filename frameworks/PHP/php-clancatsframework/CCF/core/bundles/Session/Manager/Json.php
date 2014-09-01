<?php namespace Session;
/**
 * Session File Driver
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class Manager_Json implements Manager_Interface
{
	/**
	 * Read data from the session dirver
	 *
	 * @param string		$id		The session id key.
	 * @return array
	 */
	public function read( $id )
	{
	    if ( $this->has( $id ) )
		{
			return \CCJson::read( $this->file_path( $id ) );
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
	    return file_exists( $this->file_path( $id ) );
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
	    \CCJson::write( $this->file_path( $id ), $data, true );
	}
	
	/**
	 * Delete session that are older than the given time in secounds
	 *
	 * @param int		$time
	 * @return void
	 */
	public function gc( $time )
	{
		foreach( \CCFile::ls( \CCStorage::path( 'sessions/*' ) ) as $file )
		{
			if ( ( filemtime( $file ) - ( time() - $time ) ) < 0 )
			{
				if ( !\CCFile::delete( $file ) )
				{
					throw new Exception( "Manager_File::gc - cannot delete session file." );
				}
			}
		}
	}
	
	/**
	 * Get the file path by id
	 *
	 * @param string			$id
	 * @return string
	 */
	private function file_path( $id )
	{
		return \CCStorage::path( 'sessions/'.$id.'.json' );
	}
}