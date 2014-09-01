<?php namespace Mail;
/**
 * CCMail Transporter
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class Transporter 
{
	/**
	 * Instance holder
	 *
	 * @var array
	 */
	protected static $_instances = array();
	
	/**
	 * Default transporter instance name
	 *
	 * @var string
	 */
	private static $_default = 'main';
	
	/**
	 * Get a transporter instance or create one
	 *
	 * @param string			$name
	 * @param array 			$conf	You can pass optionally a configuration directly. This will overwrite.
	 * @return Auth_Handler
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
	 * Kill an instance to force the transporter to redo the construction
	 *
	 * @return void
	 */
	public static function kill_instance( $name )
	{
		if ( array_key_exists( $name, static::$_instances ) )
		{
			unset( static::$_instances[$name] );
		}
	}
	
	/**
	 * The transporter handler name
	 *
	 * @var string
	 */
	protected $name = null;
	
	/**
	 * The transporter config array
	 *
	 * @var string
	 */
	protected $config = null;
	
	/**
	 * The transporter driver
	 *
	 * @var Transporter_Driver
	 */
	protected $driver = null;
	
	/**
	 * Transporter instance constructor
	 *
	 * @param string 		$name
	 * @param array 			$config
	 * @return void
	 */
	public function __construct( $name, $config = null ) 
	{	
		if ( is_null( $config ) )
		{
			$config = \CCConfig::create( 'mail' )->get( 'transporter.'.$name );
			
			// check for an alias. If you set a string 
			// in your config file we use the config 
			// with the passed key.
			if ( is_string( $config ) ) 
			{
				$config = \CCConfig::create( 'mail' )->get( 'transporter.'.$config );
			}
		}
		
		if ( !is_array( $config ) )
		{
			throw new Exception( "Auth\\Handler::create - Invalid auth handler (".$name.")." );
		}
		
		// also don't forget to set the name manager name becaue we need him later.
		$this->name = $name;
		
		// assign defaults and create the configuration object
		$this->config = \CCDataObject::assign( \CCArr::merge( array(
			
			// What driver should be used
			'driver' => 'sendmail',
		), $config ));
		
		
		// load the driver
		$driver_class = __NAMESPACE__.'\\Transporter_'.ucfirst( $this->config->driver );
		
		if ( !class_exists( $driver_class ) )
		{
			throw new Exception( "Invalid mail driver '".$this->config->driver."'" );
		}
		
		$this->driver = new $driver_class( $this->config );
	}
	
	/**
	 * Forward the mail to the driver
	 *
	 * @param CCMail 		$mail
	 * @return void
	 */
	public function send( CCMail $mail )
	{
		$this->driver->send( $mail );
	}
}