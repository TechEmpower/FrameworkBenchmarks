<?php namespace Core;
/**
 * String functions
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCStr 
{
	/*
	 * just some chasetes at BIN i was running out of ideas 
	 */
	const KEY 		= 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789----';
	const SECURE 	= 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789äöüÄÖÜ@<({[/=\]})>!?$%&#*-+.,;:_';
	const ALPHA_NUM 	= 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
	const ALPHA		= 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
	const ALPHA_UP	= 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
	const ALPHA_LOW	= 'abcdefghijklmnopqrstuvwxyz';
	const NUM		= '0123456789';
	const HEX		= '0123456789ABCDEF';
	const BIN		= '01';

	/**
	 * Get a charset, a string containing a set of characters.
	 *
	 * There are some predefined charsets like:
	 *     pass
	 *     secure
	 *     password
	 *     key
	 *     alphanum
	 *     alpha
	 *     alpha_low
	 *     lowercase
	 *     alpha_up
	 *     uppercase
	 *     numeric
	 *     num
	 *     hex
	 *     bin
	 * 
	 * Everything else gets returned as its own charset.
	 *
	 * @param string		$charset		use predefined charset or your own
	 * @return string
	 */
	public static function charset( $charset = null ) 
	{	
		switch( $charset ) 
		{	
			case 'pass':
			case 'secure':
			case 'password':
				return static::SECURE;
			break;

			case 'key':
				return static::KEY;
			break;

			case 'alphanum':
				return static::ALPHA_NUM;
			break;

			case 'alpha':
				return static::ALPHA;
			break;

			case 'alpha_low':
			case 'lowercase':
				return static::ALPHA_LOW;
			break;

			case 'alpha_up':
			case 'uppercase':
				return static::ALPHA_UP;
			break;

			case 'numeric':
			case 'num':
				return static::NUM;
			break;

			case 'hex':
				return static::HEX;
			break;

			case 'bin':
				return static::BIN;
			break;

			default:
				if ( !is_null( $charset ) ) 
				{
					return $charset;
				}
				return static::charset( 'alphanum' );
			break;
		}
	}

	/**
	 * Generate a random string with the given length and charset.
	 *
	 *     CCStr::random( 8, 'hex' ); // 56F6AE10
	 *     CCStr::random( 4, 'password' ); // ?F%7
	 *
	 * @param int		$length		Default is 25
	 * @param string		$charset		This parameter uses the CCStr::charset function
	 * @return string
	 */
	public static function random( $length = 25, $charset = null ) 
	{
		$charset = static::charset( $charset );

		$count = strlen( $charset ); $string = '';

		while ( $length-- ) 
		{
			$string .= $charset[mt_rand(0, $count-1)];
		}

		return $string;
	}

	/**
	 * Try to get a string from a callback reading the output buffer
	 *
	 * @param mixed		$callback
	 * @param array 		$params 
	 * @return string
	 */
	public static function capture( $callback, $params = array() )
	{
		if ( is_string( $callback ) )
		{
			return $callback;
		}

		if ( !is_closure( $callback ) )
		{
			return "";
		}

		if ( !is_array( $params ) ) 
		{
			$params = array( $params );
		}

		ob_start();
		$return = call_user_func_array( $callback,  $params );
		$buffer = ob_get_clean();

		if ( !is_null( $return ) )
		{
			return $return;
		}

		return $buffer;
	}

	/**
	 * Does the same as the PHP native htmlentities function but you can pass arrays.
	 *
	 * @param string|array			$string
	 * @param bool					$recursive
	 * @return string|array
	 */
	public static function htmlentities( $string, $recursive = false ) 
	{	
		if ( is_array( $string ) ) 
		{
			foreach( $string as $key => $item ) 
			{	
				if ( $recursive ) 
				{	
					if ( is_array( $item ) ) 
					{
						$string[$key] = static::htmlentities( $item, $recursive );
					}
				}

				if ( is_string( $item ) ) 
				{
					$string[$key] = htmlentities( $item );
				}
			}

			return $string;
		}

		return htmlentities( $string, ENT_QUOTES, ClanCats::$config->charset );
	}

	/**
	 * Get the last part of a string
	 *
	 *     CCStr::suffix( 'some-strange-file-name-2014' ); // 2014
	 *     CCStr::suffix( '/path/to/my/file.xml', '/' ); // file.xml
	 *
	 * @param string 	$string
	 * @param string		$sep			The seperator string.
	 * @return string
	 */
	public static function suffix( $string, $sep = '-' ) 
	{
		return substr( $string, strrpos( $string, $sep )+strlen( $sep ) );
	}

	/**
	 * Get the first part of a string
	 *
	 *     CCStr::prefix( 'Dave is my name', ' ' ); // Dave
	 *
	 * @param string 	$string
	 * @param string		$sep			The seperator string
	 * @return string
	 */
	public static function prefix( $string, $sep = '-' ) 
	{
		return substr( $string, 0, strpos( $string, $sep ) );
	}

	/**
	 * Alias of suffix using a dott
	 *
	 *     CCStr::extension( 'uploads/images/wallpaper.jpg' ); // jpg
	 * 
	 * @param string 	$string
	 * @return string
	 */
	public static function extension( $string ) 
	{
		return static::suffix( $string, '.' );
	}

	/**
	 * Hashs a string using the configurable method. ( main.config -> security.hash )
	 *
	 * @param string 	$string
	 * @return string
	 */
	public static function hash( $string ) 
	{
		if (is_numeric( $algo = ClanCats::$config->get( 'security.hash', 'md5') ))
		{
			return password_hash( $string, $algo );
		}
		return call_user_func( $algo, $string );
	}

	/**
	 * Verifies a hash with a plaintext string using the configurable method. ( main.config -> security.hash )
	 *
	 * @param string 	$string
	 * @param string 	$hash
	 * @return bool
	 */
	public static function verify_hash( $string, $hash ) 
	{
		if (is_numeric( $algo = ClanCats::$config->get( 'security.hash', 'md5') ))
		{
			return password_verify( $string, $hash );
		}
		return ($hash === call_user_func( $algo, $string ));
	}

	/**
	 * Cleans a string by removing special characters. "." and "-" characters are allowed by default.
	 * You can add or remove more allowed characters in the secound argument.
	 *
	 * @param string		$string
	 * @param string		$allowed
	 * @return string
	 */
	public static function clean( $string, $allowed = "\-\." ) 
	{	
		return trim( preg_replace( array( 
			'/[^A-Za-z0-9\ '.$allowed.']/',
			'/[\ ]+/'
		), array(
			'',
			' ',
		), static::replace_accents( trim( $string ) ) ) );
	}

	/**
	 * Try to form a string to url valid segment. It will remove all special characters replace
	 * accents characters remove and replace whitespaces breaks etc..
	 *
	 * @param string 	$string
	 * @param string 	$sep			You can define another seperator default is "-"
	 * @return string 
	 */
	public static function clean_url( $string, $sep = null ) 
	{
		// basic clean
		$string = strtolower( static::replace_accents( trim( $string ) ) );

		// these characters get replaced with our seperator
		$string = str_replace( array( ' ', '&', '\r\n', '\n', '+', ',' ) , '-', $string );

		$string = preg_replace( array(
			'/[^a-z0-9\-]/', // remove non alphanumerics
			'/[\-]+/', // only allow one in a row
		), array( '', '-' ), $string );

		// custom seperator
		if ( !is_null( $sep ) ) {
			$string = str_replace( '-', $sep, $string );
		}

		// trim the result again
		return trim( $string, '-' );
	}

	/**
	 * str_replace using key => value of an array
	 * 
	 * @param string		$string
	 * @param array 		$arr
	 * @param int		$count 
	 * @return string
	 */
	public static function replace( $string, $arr, $count = null ) 
	{
		return str_replace( array_keys( $arr ), array_values( $arr ), $string, $count );
	}

	/**
	 * preg replace using key => value of an array
	 * 
	 * @param string		$string
	 * @param array 		$arr
	 * @param int		$count 
	 * @return string
	 */
	public static function preg_replace( $arr, $string, $count = null ) 
	{
		return preg_replace( array_keys( $arr ), array_values( $arr ), $string, $count );
	}

	/**
	 * Converts an string to lowercase using the system encoding
	 * 
	 * @param string		$string
	 * @param string 	$encoding
	 * @return string
	 */
	public static function lower( $string, $encoding = null )
	{
		if ( is_null( $encoding ) )
		{
			$encoding = ClanCats::$config->charset;
		}
		return mb_strtolower( $string, $encoding );
	}

	/**
	 * Converts an string to uppercase using the system encoding
	 * 
	 * @param string		$string
	 * @param string 	$encoding
	 * @return string
	 */
	public static function upper( $string, $encoding = null )
	{
		if ( is_null( $encoding ) )
		{
			$encoding = ClanCats::$config->charset;
		}
		return mb_strtoupper( $string, $encoding );
	}

	/**
	 * Replace accent characters with the nearest.
	 * 
	 * @param string		$string
	 * @return string
	 */
	public static function replace_accents( $string )
	{
		return strtr( $string, array(
			'À'=>'A', 'Á'=>'A', 'Â'=>'A', 'Ã'=>'A', 'Ä'=>'Ae', 'Å'=>'A', 'Æ'=>'A', 'Ă'=>'A',
			'à'=>'a', 'á'=>'a', 'â'=>'a', 'ã'=>'a', 'ä'=>'ae', 'å'=>'a', 'ă'=>'a', 'æ'=>'ae',
			'þ'=>'b', 'Þ'=>'B',
			'Ç'=>'C', 'ç'=>'c',
			'È'=>'E', 'É'=>'E', 'Ê'=>'E', 'Ë'=>'E',
			'è'=>'e', 'é'=>'e', 'ê'=>'e', 'ë'=>'e', 
			'Ğ'=>'G', 'ğ'=>'g',
			'Ì'=>'I', 'Í'=>'I', 'Î'=>'I', 'Ï'=>'I', 'İ'=>'I', 'ı'=>'i', 'ì'=>'i', 'í'=>'i', 'î'=>'i', 'ï'=>'i',
			'Ñ'=>'N',
			'Ò'=>'O', 'Ó'=>'O', 'Ô'=>'O', 'Õ'=>'O', 'Ö'=>'Oe', 'Ø'=>'O', 'ö'=>'oe', 'ø'=>'o',
			'ð'=>'o', 'ñ'=>'n', 'ò'=>'o', 'ó'=>'o', 'ô'=>'o', 'õ'=>'o',
			'Š'=>'S', 'š'=>'s', 'Ş'=>'S', 'ș'=>'s', 'Ș'=>'S', 'ş'=>'s', 'ß'=>'ss',
			'ț'=>'t', 'Ț'=>'T',
			'Ù'=>'U', 'Ú'=>'U', 'Û'=>'U', 'Ü'=>'Ue',
			'ù'=>'u', 'ú'=>'u', 'û'=>'u', 'ü'=>'ue', 
			'Ý'=>'Y',
			'ý'=>'y', 'ý'=>'y', 'ÿ'=>'y',
			'Ž'=>'Z', 'ž'=>'z'
		)); 
	}

	/**
	 * Cuts a string after another string.
	 *
	 *     CCStr::cut( 'some/path/to/user.config.xml', '.' ); // some/path/to/user
	 *     CCStr::cut( 'some/path/to/user.config.xml', '/', false ); // some/
	 *     CCStr::cut( 'some/path/to/user.config.xml', '/', true, true ); // some/path/to
	 *
	 * @param string 	$string
	 * @param string		$key				The string that after that should be cutted.
	 * @param bool		$cut_key			Should the key itself also be removed?
	 * @param bool		$last			Cut after the last appearing of the key?
	 * @return string
 	 */
	public static function cut( $string, $key, $cut_key = true, $last = false ) 
	{
		if ( $last )
		{
			$pos = strrpos( $string, $key );
		}
		else 
		{
			$pos = strpos( $string, $key );
		}

		if ( $pos === false ) 
		{
			return $string;
		}
		if ( !$cut_key ) 
		{
			$pos += strlen( $key );
		}
		return substr( $string, 0, $pos );
	}

	/**
	 * Removes a string from another one
	 *
	 *     CCStr::strip( 'I Am Iron Man', ' ' ); // IAmIronMan
	 *     CCStr::strip( 'I Am Iron Man', 'I' ); // Am ron Man
	 *
	 * @param string 	$string
	 * @param string		$key
	 * @return string
 	 */
	public static function strip( $string, $key ) 
	{
		return str_replace( $key, '', $string );
	}

	/**
	 * Round big numbers on thousends.
	 *
	 *     CCStr::kfloor( 127861 ); // 127K
	 * 
	 * @param int 		$int
	 * @return string
	 */
	public static function kfloor( $int ) 
	{
		if ( $int >= 1000 ) 
		{
			return floor( $int / 1000 ) . 'K';
		}
		return $int;
	}

	/**
	 * Convert bytes to a human readable format
	 *
	 *     CCStr::bytes( 39247293 ); // 37.43mb
	 *
	 * @param int 		$size
	 * @return string
	 */
	public static function bytes( $size, $round = 2 ) 
	{
		$unit = array( 'b', 'kb', 'mb', 'gb', 'tb', 'pb' );
		return @round( $size / pow( 1024, ( $i = floor( log( $size, 1024 ) ) ) ), $round ).$unit[$i];
	}

	/**
	 * Convert microtime to a human readable format
	 *
	 * @param int 		$size
	 * @return string
	 */
	public static function microtime( $time, $round = 3 ) 
	{
		return round( $time, $round ).'s';
	}
}
