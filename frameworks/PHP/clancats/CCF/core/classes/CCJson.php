<?php namespace Core;
/**
 * Json helper
 * small helper for json stuff
 * 
 * the idea is to have one day a custom event based json parser
 *
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCJson {
	
	/**
	 * encode json
	 *
	 * @param array 		$array
	 * @param bool		$beautify
	 * @return string
	 */
	public static function encode( $array, $beautify = false ) 
	{	
		// php53 fix
		if ( defined( 'JSON_UNESCAPED_SLASHES' ) )
		{
			$json =  json_encode( $array, JSON_UNESCAPED_SLASHES );
		}
		else 
		{
			$json =  json_encode( $array );
		}
		
		if ( $beautify ) 
		{
			return static::beautify( $json );
		}
		
		return $json;
	}
	
	/**
	 * decode json
	 *
	 * @param string			$json
	 * @param bool			$as_array
	 * @return array|object
	 */
	public static function decode( $json, $as_array = true ) {
		return json_decode( $json, $as_array );
	}
	
	/**
	 * read a json file
	 *
	 * @param string			$path
	 * @param bool			$as_array
	 * @return array|object
	 */
	public static function read( $path, $as_array = true ) {
		return static::decode( CCFile::read( $path ), $as_array );
	}
	
	/**
	 * write a json file
	 *
	 * @param string			$path
	 * @param array 			$data
	 * @param bool			$beautify
	 * @return array|object
	 */
	public static function write( $path, $array, $beautify = false ) {
		return CCFile::write( $path, static::encode( $array, $beautify ) );
	}
	
	/** 
	 * beautifier
	 *
	 * @param string		$json
	 * @param string		$tab
	 * @param strgin		$break
	 * @return string
	 */
	public static function beautify( $json, $tab = "\t", $break = "\n" ) {
		
		$buffer = "";
		$level = 0;
		
		$in_string = false; 
		
		$len = strlen( $json ); 
		
		for($c = 0; $c < $len; $c++) { 
			
			$char = $json[$c]; 
			
			switch( $char ) { 
				case '{': 
				case '[': 
					if( !$in_string ) { 
						$buffer .= $char . "\n" . str_repeat($tab, $level+1); $level++; 
					} else { 
						$buffer .= $char; 
					} 
				break; 
				case '}': 
				case ']': 
					if( !$in_string ) { 
						$level--; $buffer .= "\n" . str_repeat($tab, $level) . $char; 
					} else { 
						$buffer .= $char; 
					} 
				break; 
				case ',': 
					if( !$in_string ) { 
						$buffer .= ",\n" . str_repeat($tab, $level); 
					} else { 
						$buffer .= $char; 
					} 
				break; 
				case ':': 
					if( !$in_string ) { 
						$buffer .= ": "; 
					} else { 
						$buffer .= $char; 
					} 
				break; 
				case '"': 
					if( $c > 0 && $json[$c-1] != '\\' ) { 
						$in_string = !$in_string; 
					} 
				default: 
					$buffer .= $char; 
				break;                    
			}
		}
		return $buffer;
	}
}