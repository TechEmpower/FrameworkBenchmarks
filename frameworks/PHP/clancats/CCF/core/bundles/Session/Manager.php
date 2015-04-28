<?php namespace Session;
/**
 * Session Manager
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class Manager extends \CCDataObject
{
	/**
	 * Instance holder
	 *
	 * @var array
	 */
	protected static $_instances = array();
	
	/**
	 * Default session instance name
	 *
	 * @var string
	 */
	private static $_default = 'main';
	
	/**
	 * Get a session instance manager
	 *
	 * @param string			$name
	 * @param array 			$conf	You can pass optionally a configuration directly. This will overwrite.
	 * @return Session\Manager
	 */
	public static function create( $name = null, $conf = null ) 
	{
		if ( is_null( $name ) ) 
		{
			$name = static::$_default;
		}
		
		if ( !is_null( $conf ) && is_array( $conf ) )
		{
			return static::$_instances[$name] = new static( $name, $conf );
		}
		
		if ( !isset( static::$_instances[$name] ) )
		{
			static::$_instances[$name] = new static( $name );
		}
		
		return static::$_instances[$name];
	}
	
	/**
	 * Some default values for our session
	 *
	 * @return array
	 */
	public static function default_data_provider() 
	{
		return array(
			'last_active'	=> time(),
			'current_lang'	=> \CCLang::current(),
			'client_agent'	=> \CCServer::client( 'agent' ),
			'client_ip'		=> \CCServer::client( 'ip' ),
			'client_port'	=> \CCServer::client( 'port' ),
			'client_lang'	=> \CCServer::client( 'language' ),
		);
	}
	
	/**
	 * The session manager name
	 *
	 * @var string
	 */
	protected $_name = null;
	
	/**
	 * The session config array
	 *
	 * @var string
	 */
	protected $_config = null;
	
	/**
	 * The session driver
	 *
	 * @var Manager_Driver
	 */
	protected $_driver = null;
	
	/**
	 * Session ID
	 *
	 * @var	string
	 */
	public $id;
	
	/**
	 * The Fingerprint
	 *
	 * @var string
	 */
	public $fingerprint;
	
	/**
	 * Session constructor
	 *
	 * @param string 		$name
	 * @param array 			$config
	 */
	protected function __construct( $name, $config = null ) 
	{
		if ( is_null( $config ) )
		{
			$config = \CCConfig::create( 'session' )->get( $name );
			
			// check for an alias. If you set a string 
			// in your config file we use the config 
			// with the passed key.
			if ( is_string( $config ) ) 
			{
				$config = \CCConfig::create( 'session' )->get( $config );
			}
		}
		
		if ( empty( $config ) )
		{
			throw new Exception( "Session\\Manager::create - Invalid session manager (".$name.")." );
		}
		
		// also don't forget to set the name manager name becaue we need him later.
		$this->_name = $name;
		
		// keep the configuration array
		$this->_config = $config;
		
		// Setup the driver class. We simply use name 
		// from the confif file and make the first letter 
		// capital. example: Handler_Mysql, Handler_Sqlite etc.
		$driver_class = __NAMESPACE__."\\Manager_".ucfirst( $config['driver'] );
		
		if ( !class_exists( $driver_class ) )
		{
			throw new Exception( "Session\\Manager::create - The driver (".$driver_class.") is invalid." );
		}
		
		$this->set_driver( $driver_class );
		
		// try to get the id from cookie
		$this->id = $this->cookie_session_id();
		
		// set the fingerprint
		$this->fingerprint = sha1( $this->id );
		
		// Before reading we might have to kill old sessions using 
		// the Garbage collector
		if ( \CCArr::get( 'gc.enabled', $this->_config, true ) )
		{
			if ( mt_rand( 1, \CCArr::get( 'gc.factor', $this->_config, 25 ) ) == 1 )
			{
				$this->gc();
			}
		}
		
		// Register a shutdown event to write the session down
		// This should not happen on shutdown if we using command line
		if ( !\ClanCats::is_cli() )
		{
			\CCEvent::mind( 'CCF.shutdown', array( $this, 'write' ) );
		}
		
		// Now get the inital data from our driver
		$this->read();
	}
	
	/**
	 * Get the current driver
	 *
	 * @return DB\Handler_Driver
	 */
	public function driver()
	{
		return $this->_driver;
	}
	
	/**
	 * Set the current driver
	 *
	 * @param string		$driver		The full driver class ( Session\Manager_ExampleDriver )
	 * @return void
	 */
	private function set_driver( $driver )
	{
		$this->_driver = new $driver( $this->_name, $this->_config );
	}
	
	/**
	 * Return the default data for the session
	 *
	 * @return array
	 */
	protected function default_data()
	{
		return call_user_func( \ClanCats::$config->get( 'session.default_data_provider' ) );
	}
	
	/**
	 * Get the current cookie name
	 *
	 * @return string 
	 */
	protected function cookie_name()
	{
		return $this->_name.\CCArr::get( 'cookie_suffix', $this->_config, '-ccf-session' );
	}
	
	/**
	 * Get the current session id from the cookie
	 *
	 * @return string
	 */
	protected function cookie_session_id()
	{
		return \CCCookie::get( $this->cookie_name(), false );
	}
	
	/**
	 * Retrive the current session fingerprint
	 *
	 * @return string
	 */
	public function fingerprint()
	{
		return $this->fingerprint;
	}
	
	/**
	 * Does the current session fingerprint match a parameter
	 *
	 * When no parameter is given we use GET->s as default parameter
	 *
	 * @param string 		$fingerprint
	 * @return string
	 */
	public function valid_fingerprint( $fingerprint = null )
	{
		if ( is_null( $fingerprint ) )
		{
			$fingerprint = \CCIn::get( \ClanCats::$config->get( 'session.default_fingerprint_parameter' ), false );
		}
		
		return $this->fingerprint === $fingerprint;
	}
	
	/**
	 * Get a value from data and remove it afterwards
	 *
	 * @param string 	$key
	 * @param mixed		$default
	 * @return mixed
	 */
	public function once( $key, $default = null ) 
	{
		$value = \CCArr::get( $key, $this->_data, $default );
		\CCArr::delete( $key, $this->_data );
		return $value;
	}
	
	/**
	 * Read data from the session driver. This overwrite's 
	 * any changes made on runtime.
	 *
	 * @return void 
	 */
	public function read() 
	{
		// Do we already have a session id if not we regenerate
		// the session and assign the default data.
		if ( $this->id ) 
		{
			if ( !$this->_data = $this->_driver->read( $this->id ) ) 
			{
				$this->regenerate();
				$this->_data = array();
			}
	
			if ( !is_array( $this->_data ) ) 
			{
				$this->_data = array();
			}
			
			$this->_data = array_merge( $this->_data, $this->default_data() );
		} 
		else 
		{
			$this->regenerate();
			$this->_data = $this->default_data();
		}
	}
	
	/**
	 * Write the session to the driver
	 *
	 * @return void
	 */
	public function write() 
	{
		$this->_driver->write( $this->id, $this->_data );
		
		// We also have to set the cookie again to keep it alive
		\CCCookie::set( $this->cookie_name(), $this->id, \CCArr::get( 'lifetime', $this->_config, \CCDate::minutes( 5 ) ) );
	}
	
	/**
	 * Generate a new session id and checks the dirver for dublicates.
	 *
	 * @return string	The new generated session id.
	 */
	public function regenerate() 
	{
		do 
		{
			$id = \CCStr::random( 32 );
		}
		while ( $this->_driver->has( $id ) );
		
		$this->fingerprint = sha1( $id );
		return $this->id = $id;
	}
	
	/**
	 * Destory the session an create a new one
	 */
	public function destroy() 
	{	
		$this->_data = $this->default_data();
		return $this->regenerate();
	}
		
	/**
	 * Garbage collection, delete all outdated sessions
	 *
	 * @return void
	 */
	public function gc() 
	{
		$lifetime = \CCArr::get( 'lifetime', $this->_config, \CCDate::minutes( 5 ) );
		
		if ( $lifetime < ( $min_lifetime = \CCArr::get( 'min_lifetime', $this->_config, \CCDate::minutes( 5 ) ) ) )
		{
			$lifetime = $min_lifetime;
		}
		
		$this->_driver->gc( $lifetime );
	}
}