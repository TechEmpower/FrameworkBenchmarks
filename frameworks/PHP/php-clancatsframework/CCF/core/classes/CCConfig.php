<?php namespace Core;
/**
 * Configuration handler
 * load, access and create configuration files
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCConfig extends CCDataObject
{	
	/**
	 * instance holder
	 *
	 * @var array
	 */
	protected static $instances = array();
	
	/**
	 * Create a configuration instance
	 *
	 * @param string			$name
	 * @return CCConfig
	 */
	public static function create( $name = null, $driver = 'file' ) 
	{
		if ( is_null( $name ) ) 
		{
			return new static( null, $driver );
		}
		
		if ( !isset( static::$instances[$name] ) ) 
		{
			static::$instances[$name] = new static( $name, $driver );
		}
		
		return static::$instances[$name];
	}
	
	/*
	 * current instance name
	 */
	protected $_instance_name = null;
	
	/*
	 * current instance name
	 */
	protected $_driver = null;
	
	/**
	 * Constructor  
	 *
	 * @param string		$name		The instance name used for writing
	 * @param string		$driver	
	 * @return void
	 */
	public function __construct( $name = null, $driver = 'file' ) 
	{	
		$this->_instance_name = $name;	
		$this->driver( $driver );
		
		if ( !is_null( $name ) ) 
		{
			$this->read( $name );
		}
	}
	
	/**
	 * Set the configuration dirver
	 *
	 * @param string			$driver
	 * @return void
	 */
	public function driver( $driver )
	{
		$driver = CCCORE_NAMESPACE.'\\'.'CCConfig_'.ucfirst( $driver );
		
		if ( !class_exists( $driver ) ) 
		{
			throw new \InvalidArgumentException("CCConfig - Invalid driver '".$driver."'");
		}
		
		$this->_driver = new $driver;
	}
	
	/**
	 * Name getter and setter
	 *
	 * @param string			$name
	 * @return string
	 */
	public function name( $name = null )
	{
		if ( is_null( $name ) )
		{
			return $this->_instance_name;
		}
		
		$this->_instance_name = $name;
	}
	
	/**
	 * Load a configuration file
	 * This will load a configuration file and assign the data
	 *
	 * @param string			$name
	 * @return void
	 */
	public function read( $name )
	{
		$this->_data = $this->_driver->read( $name );
	}
	
	/**
	 * save a configuration file
	 * this method overwrites your configuration file!!
	 *
	 * @param string 	$name
	 * @return bool
	 */
	public function write( $driver = null ) 
	{	
		if ( empty( $this->_instance_name ) ) 
		{
			throw new CCException("CCConfig::write - configuration name is missing.");
		}
		
		// set the dirver
		if ( !is_null( $driver ) )
		{
			$this->driver( $driver );
		}
		
		// run write
		$this->_driver->write( $this->_instance_name, $this->_data );
	}
	
	/**
	 * Delete the entire configuration
	 * Attention with this one he can be evil!
	 *
	 * @param string			$name
	 * @return void
	 */
	public function _delete()
	{
		if ( empty( $this->_instance_name ) ) 
		{
			throw new CCException("CCConfig::write - configuration name is missing.");
		}
		
		return $this->_driver->delete( $this->_instance_name );
	}
}