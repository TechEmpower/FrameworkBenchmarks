<?php namespace Session;
/**
 * Session Manager Driver Interface
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
interface Manager_Interface
{
    /**
     * Read data from the session dirver
     *
     * @param string		$id		The session id key.
     * @return array
     */
    public function read( $id );
    
    /**
     * Check if a session with the given key already exists
     *
     * @param string		$id		The session id key.
     * @return boool
     */
    public function has( $id );
    
    /**
     * Check if a session with the given key already exists
     *
     * @param string		$id		The session id key.
     * @param array 		$data
     * @return bool
     */
    public function write( $id, $data );
    
    /**
     * Delete session that are older than the given time in secounds
     *
     * @param int		$time
     * @return void
     */
    public function gc( $time );
}