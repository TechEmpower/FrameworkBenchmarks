<?php namespace Core;
/**
 * Language / Translations
 *
 * i've tried to write this class as simple 
 * as possible to keep the best possible
 * performence at localization.
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCLang 
{	
	/**
	 * the current language
	 *
	 * @var string
	 */
	protected static $current_language = null;
	
	/**
	 * The namespaces are like aliases or shortcuts
	 * to access language files easier.
	 *
	 * @var array
	 */
	protected static $aliases = array();
	
	/**
	 * the language data
	 * 
	 * @var array
	 */
	private static $data = array();
	
	/**
	 * static init
	 *
	 * @return void
	 */
	public static function _init() 
	{	
		static::$current_language = ClanCats::$config->get( 'language.default' );
	}
	
	/**
	 * Set or get the alias of a language file 
	 *
	 * @param string 		$alias
	 * @param string 		$path
	 * @return string|void
	 */
	public static function alias( $alias, $path = null )
	{
		if ( is_null( $path ) )
		{
			return static::$aliases[$alias];
		}
		
		static::$aliases[$alias] = $path;
	}
	
	/**
	 * Set the current language
	 * The passed string wil be parsed also the parsed / used language will be returnd.
	 * 
	 * @param string    $lang
	 * @return string
	 */
	public static function set_current( $lang ) 
	{
		$lang = static::parse( $lang );
		
		if ( !array_key_exists( $lang, static::$data ) ) 
		{
			static::$data[$lang] = array();
		}
		
		return static::$current_language = $lang;
	}
	
	/**
	 * Get the current language
	 * When $without_region is true only the first to characters will be returned.
	 * 
	 * @param bool			$without_region
	 * @return string
	 */
	public static function current( $without_region = false ) 
	{
		if ( $without_region )
		{
			return substr( static::$current_language, 0, 2 );
		}
		
		return static::$current_language;
	}
	
	/**
	 * Match an language code with the aviable languages
	 *
	 *     CCLang::parse( 'de' );
	 *     CCLang::parse( 'EN-US' );
	 *     CCLang::parse( 'de-DE,en,fr' );
	 * 
	 * @param string    $lang
	 * @return string
	 */
	public static function parse( $lang ) 
	{
		$conf = ClanCats::$config->language;
		
		if ( isset( $lang ) && strlen( $lang ) > 1 ) 
		{
			$lang = explode( ',', strtolower( $lang ) );
			$lang = explode( '-', $lang[0] );
			
			if ( !isset( $lang[1] ) ) 
			{
				$lang[1] = $lang[0];
			}
			
			$available = $conf['available'];
			
			if ( array_key_exists( $lang[0], $available ) ) 
			{
				// does even the region match?
				if ( in_array( $lang[1], $available[$lang[0]] ) ) 
				{
					return $lang[0].'-'.$lang[1];
				}
				// return the first region
				else 
				{
					$locales = $available[$lang[0]];
					return $lang[0].'-'.$locales[key($locales)];
				}
			}
		}
		
		// Return the default language when nothing coul be matched
		return $conf['default'];
	}
	
	/**
	 * Return all loaded language data
	 *
	 * @return array
	 */
	public static function raw() 
	{
		return static::$data[static::$current_language];
	}
	
	/**
	 * Load a language file into the appliaction
	 *
	 *     CCLang::load( 'some/path/to/file' );
	 *     CCLang::load( 'controller/example' );
	 *     CCLang::load( 'Blog::controller/dashboard' );
	 *
	 * @param string			$path
	 * @param bool			$overwrite
	 * @return void
	 */
	public static function load( $path, $overwrite = false ) 
	{
		if ( array_key_exists( $path, static::$data[static::$current_language] ) && $overwrite === false ) 
		{
			return;
		}
		
		$file_path = CCPath::get( $path, CCDIR_LANGUAGE.static::$current_language.'/', EXT );
		
		if ( !file_exists( $file_path ) ) 
		{
			// as fallback try to load the language file of the default language
			if ( static::$current_language !== ( $default_lang = ClanCats::$config->get( 'language.default' ) ) )
			{
				$file_path = CCPath::get( $path, CCDIR_LANGUAGE.$default_lang.'/', EXT );
				
				if ( !file_exists( $file_path ) ) 
				{
					throw new CCException( "CCLang::load - could not find language file: ".$file_path );
				}
			}
			else
			{
				throw new CCException( "CCLang::load - could not find language file: ".$file_path );
			}
		}
		
		static::$data[static::$current_language][$path] = require( $file_path );
	}
	
	/**
	 * Recive a translated line
	 *
	 *     __( 'some/path.to.my.label' );
	 *     __( 'user.welcome', array( 'name' => 'Jeff' ) )
	 *
	 * @param string			$key
	 * @param array 			$params 
	 * @return string
	 */
	public static function line( $key, $params = array() ) 
	{
		$path = substr( $key, 0, strpos( $key, '.' ) ); 
		$key = substr( $key, strpos( $key, '.' )+1 );
		
		// if there is a namespace replace the path with it
		if ( isset( static::$aliases[$path] ) )
		{
			return static::line( static::$aliases[$path].'.'.$key, $params );
		}
		
		// find the language file behind the path
		if ( !isset( static::$data[static::$current_language][$path] ) ) 
		{
			// Autoload the language file
			// The load function will throw an exception if the 
			// file doesnt exists so we dont have to care for that here.
			CCLang::load( $path );
		}
		
		// Does the line exist in the language file?
		if ( !isset( static::$data[static::$current_language][$path][$key] ) )
		{
			// We simply return the key to the user and log the missing language file line
			CCLog::add( 'CCLang::line - No such line "'.$key.'" ('.static::$current_language.') in file: '.$path, 'warning' ); return $key;
		}
		
		$line = static::$data[static::$current_language][$path][$key];
		
		// replace the params inside the line
		foreach ( $params as $param => $value ) 
		{
			$line = str_replace( ':'.$param, $value, $line );
		}
		
		return $line;
	}
}