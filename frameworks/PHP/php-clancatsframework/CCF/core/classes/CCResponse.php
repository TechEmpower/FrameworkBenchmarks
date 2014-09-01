<?php namespace Core;
/**
 * Response
 * virtual http response object
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCResponse 
{	
	/*
	 * HTTP status codes and messages by Kohana Framework: http://kohanaframework.org/
	 */
	public static $messages = array(
		// Informational 1xx
		100 => 'Continue',
		101 => 'Switching Protocols',

		// Success 2xx
		200 => 'OK',
		201 => 'Created',
		202 => 'Accepted',
		203 => 'Non-Authoritative Information',
		204 => 'No Content',
		205 => 'Reset Content',
		206 => 'Partial Content',

		// Redirection 3xx
		300 => 'Multiple Choices',
		301 => 'Moved Permanently',
		302 => 'Found', // 1.1
		303 => 'See Other',
		304 => 'Not Modified',
		305 => 'Use Proxy',
		// 306 is deprecated but reserved
		307 => 'Temporary Redirect',

		// Client Error 4xx
		400 => 'Bad Request',
		401 => 'Unauthorized',
		402 => 'Payment Required',
		403 => 'Forbidden',
		404 => 'Not Found',
		405 => 'Method Not Allowed',
		406 => 'Not Acceptable',
		407 => 'Proxy Authentication Required',
		408 => 'Request Timeout',
		409 => 'Conflict',
		410 => 'Gone',
		411 => 'Length Required',
		412 => 'Precondition Failed',
		413 => 'Request Entity Too Large',
		414 => 'Request-URI Too Long',
		415 => 'Unsupported Media Type',
		416 => 'Requested Range Not Satisfiable',
		417 => 'Expectation Failed',

		// Server Error 5xx
		500 => 'Internal Server Error',
		501 => 'Not Implemented',
		502 => 'Bad Gateway',
		503 => 'Service Unavailable',
		504 => 'Gateway Timeout',
		505 => 'HTTP Version Not Supported',
		509 => 'Bandwidth Limit Exceeded'
	);
	
	/*
	 * status
	 */
	protected $_status = 200;
	
	/*
	 * body
	 */
	protected $_body = null;
	
	/*
	 * header
	 */
	protected $_header = array();
	
	
	/**
	 * response factory 
	 * create new responses
	 * 
	 * @param string 	$body
	 * @param int		$status
	 * @return CCResponse
	 */
	public static function create( $body = null, $status = 200 ) 
	{		
		return new static( $body, $status );
	}
	
	/**
	 * error response
	 * executes an private route with the error status code and 
	 * delivers the resulting response
	 *
	 * @param int 		$status
	 * @return CCResponse
	 */
	public static function error( $status ) 
	{
		return CCRequest::uri( '#'.$status )->perform()->response();
	}
	
	/**
	 * json response
	 * you can pass an array of data wich will be converted to json
	 *
	 * @param array 			$data
	 * @param int			$status
	 * @param bool 			$beautify	should the json output be formatted?
	 * @return CCResponse
	 */
	public static function json( $data = array(), $status = 200, $beautify = true ) 
	{
		if ( !is_array( $data ) )
		{
			throw new CCException( "CCResponse::json - first argument has to be an array." );
		}
		
		$response = new static( CCJson::encode( $data, $beautify ), $status );
		$response->header( 'Content-Type', 'text/json' );
		
		return $response;
	}
	
	/**
	 * download response
	 * creates an respose that forces the browser to download as file
	 *
	 * @param string 		$body
	 * @param string 		$filename
	 * @param int			$status
	 * @return CCResponse
	 */
	public static function download( $body, $filename = null, $status = 200 ) 
	{
		return static::create( $body, $status )->as_download( $filename );
	}
	
	/**
	 * response constructor
	 *
	 * @param string 	$body
	 * @param int		$status
	 */
	public function __construct( $body, $status ) 
	{
		$this->_body = $body;
		$this->_status = $status;
	}
	
	/**
	 * magic get
	 *
	 * @param string		$key 
	 * @return mixed
	 */
	public function & __get( $key ) 
	{
		if ( method_exists( $this, $key ) )
		{
			return call_user_func( array( $this, $key ) );
		}
	}
	
	/**
	 * magic set
	 *
	 * @param string		$key
	 * @param mixed		$value
	 * @return void
	 */
	public function __set( $key, $value ) 
	{
		if ( method_exists( $this, $key ) )
		{
			return call_user_func( array( $this, $key ), $value );
		}
	}
	
	/**
	 * body setter and getter
	 *
	 * @param string|null	$body
	 * @return string|self
	 */
	public function body( $body = null ) 
	{
		if ( !is_null( $body ) ) 
		{
			$this->_body = $body; return $this;
		}
		
		return $this->_body;
	}
	
	/**
	 * status setter and getter
	 *
	 * @param int|null		$code
	 * @return int|self
	 */
	public function status( $code = null ) 
	{
		if ( !is_null( $code ) ) 
		{
			if ( headers_sent() && !CLI ) 
			{
				throw new CCException( "CCResponse::status - cannot change status header has already been send." );
			}
			
			$this->_status = $code; return $this;
		}
		
		return $this->_status;
	}
	
	/**
	 * status setter and getter
	 *
	 * @param string				$key
	 * @param string|null		$str
	 * @return string|self
	 */
	public function header( $key, $str = null ) 
	{
		if ( !is_null( $str ) ) 
		{
			$this->_header[$key] = $str; return $this;
		}
		
		return $this->_header[$key];
	}
	
	/**
	 * modify the headers to force a download
	 *
	 * @param string 	$filename
	 * @return void
	 */
	public function as_download( $filename = null ) 
	{
		if ( is_null( $filename ) ) 
		{
			$filename = 'file.'.CCStr::suffix( $this->header( 'Content-Type' ), '/' );
		}
		
		$this->header( 'Content-Description', 'File Transfer' );
		$this->header( 'Content-Disposition', 'attachment; filename='.$filename );
		$this->header( 'Content-Transfer-Encoding', 'binary' );
		$this->header( 'Expires', '0' );
		$this->header( 'Cache-Control', 'must-revalidate' );
		$this->header( 'Pragma', 'public' );
		$this->header( 'Content-Length', strlen( $this->body() ) );
		
		return $this;
	}
	
	/**
	 * send response
	 * means printing the response and setting the headers if set
	 *
	 * @param bool		$headers	
	 * @return  void
	 */
	public function send( $headers = false ) 
	{
		if ( $headers && headers_sent() && !CLI ) 
		{
			throw new CCException( "CCResponse::send - cannot send header, header has already been send." );
		}
		
		if ( $headers ) 
		{
			// status header
			header( CCIn::server( 'SERVER_PROTOCOL' ).' '.$this->_status.' '.CCResponse::$messages[$this->_status] );
			
			// check if content type is already set
			if ( !isset( $this->_header['Content-Type'] ) ) 
			{
				$this->header( 'Content-Type', 'text/html; charset='.ClanCats::$config->get( 'charset', 'utf-8' ) );
			}

			$this->header( 'X-Powered-By', 'ClanCatsFramework version: '.ClanCats::VERSION );
		
			// set headers
			foreach( $this->_header as $key => $content ) 
			{
				header( $key.': '.$content );
			}
		}
		
		// profiler
		CCProfiler::check( 'CCResponse - sending response' );
		
		// print the body
		echo CCEvent::pass( 'response.output', $this->_body );
	}
}