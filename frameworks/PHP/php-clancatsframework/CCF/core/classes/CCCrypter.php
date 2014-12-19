<?php namespace Core;
/**
 * Crypter
 * en / decrypt data with a salt
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCCrypter {
	
	/*
	 * default salt
	 */
	protected static $default_salt;
	
	/**
	 * static initialisation
	 * @return void
	 */
	public static function _init() {
		static::$default_salt = ClanCats::$config->get( 'security.salt', 'bitch4please' );
	}
	
	/**
	 * Encode data
	 *
	 * @param string	$data
	 * @param string 	$salt 
	 * @return string
	 */
	public static function encode( $data, $salt = null ) {
		
		if ( is_null( $salt ) ) {
			$salt = static::$default_salt;
		}
		
		return base64_encode(mcrypt_encrypt(MCRYPT_RIJNDAEL_256, md5($salt), $data, MCRYPT_MODE_CBC, md5(md5($salt))));
	}
	
	/**
	 * Decde data
	 *
	 * @param string 	$data
	 * @param string 	$salt
	 */
	public static function decode( $data, $salt = null ) {
		
		if ( is_null( $salt ) ) {
			$salt = static::$default_salt;
		}
		
		return rtrim(mcrypt_decrypt(MCRYPT_RIJNDAEL_256, md5($salt), base64_decode($data), MCRYPT_MODE_CBC, md5(md5($salt))), "\0");
	}
}