<?php namespace CC\Core;
/**
 * ClanCats HTTP Client
 *
 * @package 		ClanCats-Framework
 * @author     		Mario Döring <mariodoering@me.com>
 * @version 		0.5
 * @copyright 		2010 - 2013 ClanCats GmbH 
 *
 */
class CCHTTP {
	
	/*
	 * bot version
	 */
	const BOT_VERSION = '1.0';
	
	/*
	 * Request types
	 */
	const get 		= 'GET';
	const post 		= 'POST';
	const put 		= 'PUT';
	const delete 	= 'DELETE';
	
	/**
	 * create new get request
	 *
	 * @param string	$url
	 * @param array 	$headers  
	 */
	public static function get( $url, $headers = array() ) {
		return new static( static::get, $url, $headers );
	}
	
	/**
	 * create new post request
	 *
	 * @param string	$url
	 * @param array 	$headers
	 * @param array 	$data
	 */
	public static function post( $url, $headers = array(), $data = array() ) {
		return new static( static::post, $url, $headers, $data );
	}
	
	/**
	 * create new put request
	 *
	 * @param string	$url
	 * @param array 	$headers  
	 * @param array 	$data
	 */
	public static function put( $url, $headers = array(), $data = array() ) {
		return new static( static::put, $url, $headers, $data );
	}
	
	/**
	 * create new delete request
	 *
	 * @param string	$url
	 * @param array 	$headers  
	 */
	public static function delete( $url, $headers = array() ) {
		return new static( static::delete, $url, $headers );
	}
	
	
	/*
	 * the request type
	 */
	private $request_type = null;
	
	/*
	 * the request url
	 */
	public $url = null;
	
	/*
	 * request cookies
	 */
	public $cookies = array();
	
	/*
	 * request data
	 */
	public $data = array();
	
	/*
	 * request headers
	 */
	protected $_headers = array();
	
	/*
	 * strtolower the header keys?
	 */
	public $_normalize_header_keys = true;
	
	/**
	 * HTTP Request constructor
	 *
	 * @param string	$type
	 * @param string	$url
	 * @param array 	$headers  
	 * @param array 	$data
	 */
	public function __construct( $type, $url, $headers, $data ) {
		$this->type = $type;
		$this->url = $url;
		$this->data = $data;
		
		// default useragent
		$this->header( 'user-agent', 'ClanCatsBot/'.static::BOT_VERSION.' (+http://www.clancats.com/infobot.html)' );
		
		foreach( $headers as $key => $header ) {
			$this->header( $key, $header );
		}
	}
	
	/**
	 * return the current host from the url
	 * @return string
	 */
	public function host() {
		return parse_url( $this->url, PHP_URL_HOST );
	}
	
	/**
	 * header getter and setter
	 *
	 * @param string 	$key
	 * @param mixed		$value
	 */
	public function header( $key, $value = null ) {
		
		$key = explode( '-', $key );
		foreach( $key as $k => $v ) {
			$key[$k] = ucfirst( strtolower($v));
		}
		$key = implode( '-', $key );
		
		if ( !is_null( $value ) ) {
			$this->_headers[$key] = $value;
		}
		
		return $this->_headers[$key];
	}
	
	/**
	 * and finally execute the request
	 *
	 * returns false if the request fails
	 *
	 * @param string	$what | both, header, body
	 * @return mixed
	 */
	public function request( $what = 'both' ) {
		
		/*
		 * prepare curl
		 */
		$ch = curl_init( $this->url );
		curl_setopt( $ch, CURLOPT_RETURNTRANSFER, 1 );  
		//curl_setopt( $ch, CURLOPT_VERBOSE, 1 );
		curl_setopt( $ch, CURLOPT_HEADER, 1 );
		curl_setopt( $ch, CURLOPT_FOLLOWLOCATION, 1 );
	
		/*
		 * prepare the headers
		 */
		$headers = array();
		foreach( $this->_headers as $key => $header ) {
			$headers[] = $key.': '.$header;
		}
		curl_setopt( $ch, CURLOPT_HTTPHEADER, $headers );
		
		/*
		 * post data
		 */
		if ( $this->type == static::post || $this->type == static::put ) {
			curl_setopt( $ch, CURLOPT_POST, 1 );  
			curl_setopt( $ch, CURLOPT_POSTFIELDS, $post_data ); 
		}
		
		/*
		 * execute that request
		 */
		$response = curl_exec( $ch );
		if( $response === false ) {
			//echo 'Curl error: ' . curl_error($ch); 
			return false;
		}
		$header_size = curl_getinfo( $ch, CURLINFO_HEADER_SIZE );
		$header = substr( $response, 0, $header_size );
		$body = substr( $response, $header_size ); 
		
		// close it
		curl_close($ch);
		
		// format the header
		$arr_header = array();
		$header = explode( "\n", $header );
		foreach( $header as $item ) {
			$head_part = explode( ":", $item );
			if ( $this->_normalize_header_keys ) {
				$arr_header[strtolower( trim($head_part[0]) )] = trim($head_part[1]);
			}
			else {
				$arr_header[trim($head_part[0])] = trim($head_part[1]);
			}
		}
		
		/*
		 * return it
		 */
		if ( $what == 'both' ) {
			$return = new \stdClass;
			$return->body = $body;
			$return->header = $arr_header;
		}
		elseif ( $what == 'body' ) {
			$return = $body;
		}
		elseif ( $what == 'header' ) {
			$return = $arr_header;
		}
		else {
			throw new CCException( 'CCHTTP - param 1 in request function is invalid! Allowed are: both, header, body' );
		}
		return $return;
	} 
}