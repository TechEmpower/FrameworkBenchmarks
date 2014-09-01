<?php namespace Core;
/**
 * Input instance
 * input data including client information POST / GET params ect...
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCIn_Instance {
	
	/*
	 * GET params
	 */
	public $GET;
	
	/*
	 * POST params
	 */
	public $POST;
	
	/*
	 * recived cookies
	 */
	public $COOKIE;
	
	/*
	 * server data
	 */
	public $SERVER;
	
	/*
	 * recived files
	 */
	public $FILES;
	
	
	/*
	 * client data
	 */
	public $client;
	
	/*
	 * the current uri
	 */
	public $uri;
	
	/**
	 * instance constructor
	 * assign the main vars GET, POST, COOKIE, FILES, SERVER here
	 * 
	 * @param array 		$get
	 * @param array 		$post
	 * @param array 		$cookie
	 * @param array 		$files
	 * @param array 		$server
	 */ 
	public function __construct( $get, $post, $cookie, $files, $server ) 
	{
		$this->GET = $get;
		$this->POST = $post;
		$this->COOKIE = $cookie;
		$this->FILES = $files;
		$this->SERVER = $server;
	}
	
	/**
	 * get a GET param
	 *
	 * @param string		$key
	 * @param mixed		$default
	 * @return mixed
	 */
	public function get( $key, $default = null ) 
	{
		if ( !isset( $this->GET[$key] ) ) 
		{
			return $default;
		}
		return $this->GET[$key];
	}
	
	/**
	 * has a GET param
	 *
	 * @param string		$key
	 * @return mixed
	 */
	public function has_get( $key ) 
	{
		return array_key_exists( $key, $this->GET );
	}

	/**
	 * get a POST param
	 *
	 * @param string		$key
	 * @param mixed		$default
	 * @return mixed
	 */
	public function post( $key, $default = null ) 
	{
		if ( !isset( $this->POST[$key] ) ) 
		{
			return $default;
		}
		return $this->POST[$key];
	}
	
	/**
	 * has a POST param
	 *
	 * @param string		$key
	 * @return mixed
	 */
	public function has_post( $key ) 
	{
		return array_key_exists( $key, $this->POST );
	}

	/**
	 * get a SERVER param
	 *
	 * @param string		$key
	 * @return mixed
	 */
	public function server( $key, $default = null ) {
		if ( !isset( $this->SERVER[strtoupper( $key )] ) ) 
		{
			return $default;
		}
		return $this->SERVER[strtoupper( $key )];
	}
	
	/**
	 * has a SERVER param
	 *
	 * @param string		$key
	 * @return mixed
	 */
	public function has_server( $key ) 
	{
		return array_key_exists( strtoupper($key), $this->SERVER );
	}
	
	/**
	 * get a FILE param
	 *
	 * @param string		$key
	 * @param mixed		$default
	 * @return mixed
	 */
	public function file( $key, $default = null ) 
	{
		if ( !isset( $this->FILES[$key] ) ) 
		{
			return $default;
		}
		return $this->FILES[$key];
	}
	
	/**
	 * has a FILE param
	 *
	 * @param string		$key
	 * @return mixed
	 */
	public function has_file( $key ) 
	{
		return array_key_exists( $key, $this->FILES );
	}

	/**
	 * get the client data
	 *
	 * @param string		$key
	 * @return mixed
	 */
	public function client( $key = null ) {

		// check if we already set the client object
		if ( is_null( $this->client ) ) 
		{
			// make client 
			$this->client = new \stdClass;

			/*
			 * get clients ip address
			 */
			// Cloudlfare fix 
			if ( $this->has_server( 'HTTP_CF_CONNECTING_IP' ) ) 
			{
				$this->client->ip = $this->server( 'HTTP_CF_CONNECTING_IP' );
			}
			// proxy?
			elseif ( $this->has_server( 'HTTP_X_FORWARDED_FOR' ) ) 
			{
				$this->client->ip = $this->server( 'HTTP_X_FORWARDED_FOR' );
			} 
			// another proxy?
			elseif ( $this->has_server( 'HTTP_CLIENT_IP' ) )
			{
				$this->client->ip = $this->server( 'HTTP_CLIENT_IP' );
			}
			// or default
			else 
			{
				$this->client->ip = $this->server( 'REMOTE_ADDR', '127.0.0.1' );
			}

			/*
			 * set clients user agent
			 */
			$this->client->agent = $this->server( 'HTTP_USER_AGENT', '' );

			/*
			 * set clients port
			 */
			$this->client->port = $this->server( 'REMOTE_PORT', '' );

			/*
			 * set clients fingerprint based on host and agent
			 */
			$this->client->fingerprint = CCStr::hash( $this->client('agent').$this->client('ip') );

			/*
			 * set clients language
			 */
			$this->client->language = CCLang::set_current( $this->server( 'HTTP_ACCEPT_LANGUAGE' ) );
		}

		// return the object
		if ( is_null( $key ) ) 
		{
			return $this->client;
		}

		// return a special key
		return $this->client->{$key};
	}

	/**
	 * get the current requesting method
	 * GET, POST, PUT, DELETE
	 *
	 * @param string			$is
	 * @return string 
	 */
	public function method( $is = null ) 
	{
		if ( !is_null( $is ) )
		{
			return strtoupper( $is ) === $this->method();
		}
		
		return strtoupper( $this->server( 'HTTP_X_HTTP_METHOD_OVERRIDE', $this->server( 'REQUEST_METHOD', 'GET' ) ) );
	}
	
	/**
	 * get the current protocol
	 *
	 * @return string 
	 */
	public function protocol() 
	{
		return ( ( $this->has_server( 'HTTPS' ) && $this->server( 'HTTPS' ) != 'off' ) || $this->server( 'SERVER_PORT' ) == 443 ) ? 'https' : 'http';
	}
	
	/**
	 * get the current host
	 *
	 * @return string 
	 */
	public function host() 
	{
		return $this->server( 'HTTP_X_FORWARDED_HOST', $this->server( 'HTTP_HOST', $this->server( 'SERVER_NAME' ) ) );
	}

	/**
	 * get the current server software SERVER:SERVER_SOFTWARE
	 *
	 * @return string 
	 */
	public function software() 
	{
		return $this->server( 'SERVER_SOFTWARE' );
	}
	
	/**
	 * get the http referrer SERVER:HTTP_REFERER
	 *
	 * @return string 
	 */
	public function referrer() 
	{
		return $this->server( 'HTTP_REFERER' );
	}

	/**
	 * get the requestet uri
	 *
	 * @param bool		$full	Don't cut the get params
	 * @return string 
	 */
	public function uri( $full = false ) 
	{
		// check if we already set the current uri
		if ( is_null( $this->uri ) ) 
		{
			$this->uri = $this->server('REQUEST_URI', '/' );
			
			// fix doubled slashes
			$this->uri = preg_replace( '/(\/+)/','/', $this->uri );
			
			// remove get params
			if ( !$full ) 
			{
				$this->uri = CCStr::cut( $this->uri, '?' );
			}
			
			// cut the offset from the config
			$this->uri = substr( $this->uri, strlen( ClanCats::$config->get( 'url.path' ) ) );
			
			// if null or false set to default
			if ( !$this->uri ) 
			{
				$this->uri = '';
			}
		}
		return $this->uri;
	}
	
	/**
	 * get the requestet url
	 *
	 * @param bool		$full	Don't cut the get params
	 * @return string 
	 */
	public function url() 
	{
		return static::protocol().'://'.static::host().$this->server('REQUEST_URI', '/' );
	}
	
	/**
	 * is the current request an ajax request?
	 *
	 * @return bool
	 */
	public function is_ajax() 
	{
		return $this->has_server( 'HTTP_X_REQUESTED_WITH' );
	}
}