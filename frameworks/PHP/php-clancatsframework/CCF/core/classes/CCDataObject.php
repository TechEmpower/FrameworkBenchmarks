<?php namespace Core;
/**
 * Data Object
 * The DataObject is an class that simply holds data that are aviable via magic getters and setters.
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCDataObject 
{	
	/**
	 * creates an data object and assigns passed data
	 *
	 * @param array 		$data
	 * @return new static
	 */
	public static function assign( $data ) 
	{
		$object = new static;
		$object->_data = $data;
		
		return $object;
	}
    
    /*
     * the data holder.
     */
    public $_data = array();
	
	/**
	 * set data to the object.
	 *
	 * @param string 	$key
	 * @param mixed		$value
	 * @param mixed 		$param
	 *
	 * @return void
	 */
	public function set( $key, $value, $param = null ) 
	{
	    CCArr::set( $key, $value, $this->_data );
	}
	
	/**
	 * Binds the var in the first dimension.
	 *
	 * @param string 	$key
	 * @param mixed		$value
	 *
	 * @return void
	 */
	public function bind( $key, &$value) 
	{
		$this->_data[$key] = &$value;
	}
	
	/**
	 * Add data to the object
	 *
	 * @param string 	$key
	 * @param mixed		$value
	 *
	 * @return void
	 */
	public function add( $key, $value ) 
	{
		CCArr::add( $key, $value, $this->_data );
	}
	
	/**
	 * gets data, use a default if not set.
	 *
	 * @param string 	$key
	 * @param mixed		$default
	 * @return mixed
	 */
	public function get( $key, $default = null ) 
	{
	    return CCArr::get( $key, $this->_data, $default );
	}
	
	/**
	 * check if data exsists for key
	 *
	 * @param string 	$key
	 * @return mixed
	 */
	public function has( $key ) 
	{
		return CCArr::has( $key, $this->_data );
	}
	
	/**
	 * delete data from our array
	 *
	 * @param string 	$key
	 * @return mixed
	 */
	public function delete( $key ) 
	{
		CCArr::delete( $key, $this->_data );
	}
	
	/**
	 * return the raw data of the object
	 *
	 * @return array
	 */
	public function raw() 
	{
		return $this->_data;
	}
	
	/**
	 * magic get a value from the object
	 *
	 * @param $key 
	 * @return mixed
	 */
	public function & __get( $key ) 
	{
	    return $this->_data[$key];
	}
	
	/**
	 * magic set data to the object.
	 *
	 * @param $key
	 * @param $value
	 * @return void
	 */
	public function __set( $key, $value ) 
	{
	    $this->_data[$key] = $value;
	}
	
	/**
	 * magic check if data isset
	 *
	 * @param $key
	 * @return bool
	 */
	public function __isset( $key ) 
	{
	    return isset( $this->_data[$key] );
	}
	
	/**
	 * magic delete data  
	 *
	 * @param $key
	 * @return void
	 */
	public function __unset( $key ) 
	{
	    unset( $this->_data[$key] );
	}
}
