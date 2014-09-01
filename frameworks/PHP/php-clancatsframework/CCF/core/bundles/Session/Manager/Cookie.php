<?php namespace Session;
/**
 * Session Cookie Driver
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class Manager_Cookie implements Manager_Interface
{
	/**
	 * Cookie suffix string
	 *
	 * @var string
	 */
	private $cookie_suffix = null;
	
	/**
	 * Salt for crypting the session 
	 *
	 * @var string
	 */
	private $crypt_salt = null;
	
	/**
	 * Cookie driver constructor
	 */
	public function __construct( $name, $conf )
	{
		$this->cookie_suffix = \CCArr::get( 'cookie_suffix', $conf, '-session-store' );
		$this->crypt_salt = \CCArr::get( 'crypt_salt', $conf );
	}
	
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
			return json_decode( \CCCrypter::decode( \CCCookie::get( $id.$this->cookie_suffix ), $this->crypt_salt ), true );
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
	    return (bool) \CCCookie::get( $id.$this->cookie_suffix, false );
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
		\CCCookie::set( $id.$this->cookie_suffix, \CCCrypter::encode( json_encode( $data ), $this->crypt_salt ) );
	}
	
	/**
	 * Delete session that are older than the given time in secounds
	 *
	 * @param int		$time
	 * @return void
	 */
	public function gc( $time )
	{
		return false; // There is no garbage collection when using cookies
	}
}