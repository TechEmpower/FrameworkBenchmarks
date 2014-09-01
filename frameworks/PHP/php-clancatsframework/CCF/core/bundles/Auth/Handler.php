<?php namespace Auth;
/**
 * Auth instnace handler 
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
 
use Core\CCCookie;
 
class Handler
{
	/**
	 * Instance holder
	 *
	 * @var array
	 */
	protected static $_instances = array();

	/**
	 * Default auth instance name
	 *
	 * @var string
	 */
	private static $_default = 'main';

	/**
	 * Get an auth instance or create one
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
	 * Kill an instance to force the handler to redo the construction
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
	 * the user object
	 *
	 * @var DB\Model
	 */
	public $user = null;
	
	/**
	 * is the instance authenticated
	 *
	 * @var bool
	 */
	protected $authenticated = false;
	
	/**
	 * The auth handler name
	 *
	 * @var string
	 */
	protected $name = null;
	
	/**
	 * The auth config array
	 *
	 * @var string
	 */
	protected $config = null;
	
	/**
	 * The used session manager
	 *
	 * @var string
	 */
	public $session = null;
	
	/**
	 * Auth instance constructor
	 *
	 * @param string 		$name
	 * @param array 			$config
	 * @return void
	 */
	public function __construct( $name, $config = null ) 
	{	
		if ( is_null( $config ) )
		{
			$config = \CCConfig::create( 'auth' )->get( $name );
			
			// check for an alias. If you set a string 
			// in your config file we use the config 
			// with the passed key.
			if ( is_string( $config ) ) 
			{
				$config = \CCConfig::create( 'auth' )->get( $config );
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
			
			// Wich session manager should be used?
			// null = default session manager
			'session_manager' => null,
			
			// On wich field should the current logged in user
			// id be saved in the session?
			'session_key' => 'user_id',
			
			// On wich field do we select the user for 
			// the authentification
			'user_key' => 'id',
			
			// The User model class
			'user_model' => "\\Auth\\User",
			
			// the identifiers wich fields should be allowed 
			// to select the user object on validation.
			'identifiers' => array(
				'email'
			),
			
			// Where to store the active logins
			// how long do they stay active etc.
			'logins' => array(
			
				// the logins db handlerw
				'handler' => null,
			
				// the logins db table
				'table' => 'auth_logins',
			),
			
			// login restoring settings
			'restore' => array(
				
				// the user id key cookie name
				'id_cookie' => 'ccauth-restore-id',
				
				// the user restore token cookie name
				'token_cookie' => 'ccauth-restore-token',
				
				// the restore key lifetime
				'lifetime' => \CCDate::months(1),
			),
			
		), $config ));
				
		// set the session handler
		$this->session = \CCSession::manager( $this->config->session_manager );
		
		$user_model = $this->config->user_model;
		
		// set a empty default user object to avoid
		// on a non object errors
		$this->user = new $user_model;
		
		// do we already have a user id means are we
		// logged in?
		if ( !is_null( $session_key = $this->session_user_id() ) )
		{
			if ( $user = $user_model::find( $this->config->user_key, $session_key ) )
			{
				$this->user = $user; return $this->authenticated = true;
			}
		}
		// When no session key / user id is given try to restore 
		// the login using the login keepers
		else 
		{
			$restore_id_cookie = $this->config->get( 'restore.id_cookie' );
			$restore_token_cookie = $this->config->get( 'restore.token_cookie' );
			
			if 
			(
				CCCookie::has( $restore_id_cookie ) &&
				CCCookie::has( $restore_token_cookie )
			) 
			{
				// get the restore cookies
				$restore_id = CCCookie::get( $restore_id_cookie );
				$restore_token = CCCookie::get( $restore_token_cookie );
	
				// get the restore login
				$login = $this->select_logins()
					->where( 'restore_id', $restore_id )
					->where( 'restore_token', $restore_token )
					->limit( 1 );
	
				// if no login found kill the cookies and return
				if ( !$login = $login->run() ) 
				{
					$this->kill_restore();
					return $this->authenticated = false;
				}
	
				// Invalid user? kill the cookies and return
				if ( !$user = $user_model::find( $this->config->user_key, $restore_id ) )
				{
					$this->kill_restore();
					return $this->authenticated = false;
				}
	
				// validate the restore key if invalid 
				// once again kill the cookies and return
				if ( $login->restore_token != $this->restore_key( $user ) ) 
				{
					$this->kill_restore();
					return $this->authenticated = false;
				}
	
				// If everything is fine sign the user in and 
				// update the restore keys
				$this->sign_in( $user, true );
				
				return $this->authenticated = true;
			}
		}
	
		return $this->authenticated = false;
	}
	
	/**
	 * Kill the restore keys
	 *
	 * @return void
	 */
	public function kill_restore()
	{
		CCCookie::delete( $this->config->get( 'restore.id_cookie' ) );
		CCCookie::delete( $this->config->get( 'restore.token_cookie' ) );
	}
	
	/**
	 * Is this login valid?
	 *
	 * @return bool
	 */
	public function valid()
	{
		return $this->authenticated;
	}
	
	/**
	 * Get the current user session user id 
	 * 
	 * @return mixed
	 */
	public function session_user_id() 
	{
		return $this->session->get( $this->config->session_key );
	}
	
	/**
	 * generate the current restore key
	 *
	 * @param User	$user
	 * @return string
	 */
	public function restore_key( $user ) 
	{
		return \CCStr::hash( $user->password.'@'.$user->{$this->config->user_key}.'%'.\CCIn::client()->ip );
	}
	
	/**
	 * Select from logins
	 *
	 * @return DB\Query_Select
	 */
	private function select_logins()
	{
		return \DB::select( 
			$this->config->get( 'logins.table' ), 
			array(),
			$this->config->get( 'logins.handler' )
		);
	}
	
	/**
	 * Get the current login of the user
	 *
	 * @return stdObject|null
	 */
	public function login()
	{
		return $this->select_logins()
			->where( 'restore_id', $this->user->{$this->config->user_key} )
			->where( 'restore_token', $this->restore_key( $this->user ) )
			->limit( 1 )
			->run();
	}
	
	/**
	 * Validate an identifier with the password 
	 * In other words is the login correct?
	 *
	 * @param string 	$identifier
	 * @param string 	$password
	 * @return mixed  	false on failure, user object on success
	 */
	public function validate( $identifier, $password ) 
	{
		$user = null;
		$user_model = $this->config->user_model;
		
		foreach( $this->config->identifiers as $property ) 
		{
			if ( $user = $user_model::find( $property, $identifier ) ) 
			{
				break;
			} 
		}
	
		// when could not find a user matching the identifiers return false
		if ( !$user ) 
		{
			return false;
		}
		
		// when the passwords match return the user object
		if ( \CCStr::verify_hash( $password, $user->password )) 
		{
			return $user;
		}
	
		// otherwise return false
		return false;
	}
	
	/**
	 * Sign the user and optinal also set the resore keys
	 *
	 * @param Auth\User  	$user	
	 * @param bool			$keep_login
	 * @return bool
	 */
	public function sign_in( \Auth\User $user, $keep_login = true ) 
	{
		// set the session key so the session knows we are logged in
		$this->session->set( $this->config->session_key, $user->{$this->config->user_key} );
		
		// update the current user object
		$this->user = $user;
		
		// update the last login timestamp
		$this->user->last_login = time();
		
		// pass the user trough the events to allow modifications
		// of the user object at sign in
		$this->user = \CCEvent::pass( 'auth.sign_in', $this->user );
		
		// save the user object to the database 
		$this->user->save();
		
		// set the restore keys to keep the login
		// after the session ends
		if ( $keep_login ) 
		{
			$restore_id_cookie = $this->config->get( 'restore.id_cookie' );
			$restore_token_cookie = $this->config->get( 'restore.token_cookie' );
			
			$restore_lifetime = $this->config->get( 'restore.lifetime' );
			
			$restore_id = $this->session->get( $this->config->session_key );
			$restore_token = $this->restore_key( $this->user );
			
			CCCookie::set( $restore_id_cookie, $restore_id, $restore_lifetime );
			CCCookie::set( $restore_token_cookie, $restore_token, $restore_lifetime );
			
			// try to get the current login
			$login = $this->select_logins()
				->where( 'restore_id', $restore_id )
				->where( 'restore_token', $restore_token );
		
			// prepare the login data
			$login_data = array(
				'restore_id' 		=> $restore_id,
				'restore_token' 		=> $restore_token,
				'last_login' 		=> time(),
				'client_agent'		=> \CCIn::client()->agent,
			);
			
			// pass the login data trough the events
			$login_data = \CCEvent::pass( 'auth.store_login', $login_data );
			
			// if there is no such login create a new one
			if ( !$login->run() ) 
			{
				\DB::insert( $this->config->get( 'logins.table' ), $login_data )
					->run( $this->config->get( 'logins.handler' ) );
			}
			else 
			{
				\DB::update( $this->config->get( 'logins.table' ), $login_data )
					->where( 'restore_id', $restore_id )
					->where( 'restore_token', $restore_token )
					->run( $this->config->get( 'logins.handler' ) );
			}
		}
		
		// and finally we are authenticated
		return $this->authenticated = true;
	}
	
	/**
	 * Sign a user out
	 *
	 * @param id  				$user_id	
	 * @param string				$name
	 * @return false
	 */
	public function sign_out() {
	
		if ( !$this->authenticated ) 
		{
			return false;
		}
	
		// remove the restore login
		\DB::delete( $this->config->get( 'logins.table' ) )
			->where( 'restore_token', $this->restore_key( $this->user ) )
			->run();
	
		// logout the user
		$this->session->delete( $this->config->session_key );

		// pass the user object through all user hooks
		$this->user = \CCEvent::pass( 'auth.sign_out', $this->user );
		$this->user->save();
	
		$user_model = $this->config->user_model;
		
		// create new empty user
		$this->user = new $user_model;
	
		return $this->authenticated = false;
	}
}
