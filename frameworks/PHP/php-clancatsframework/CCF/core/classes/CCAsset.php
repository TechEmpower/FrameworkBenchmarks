<?php namespace Core;
/**
 * Asset handler 
 * This is helper your front end stuff like js file, images etc..
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCAsset 
{	
	/**
	 * instance holder
	 *
	 * @var array
	 */
	protected static $instances = array();
	
	/**
	 * default instance
	 * 
	 * @var string
	 */
	protected static $_default = 'main';
	
	/**
	 * The macros holder
	 *
	 * @var array
	 */
	protected static $_macros = array();
	
	/**
	 * CSS Macro - Generates a css stylesheet link
	 * 
	 * @param string 			$uri
	 * @param string 			$holder
	 * @return string
	 */
	protected static function macro_css( $uri, $holder = null )
	{
		return '<link type="text/css" rel="stylesheet" href="'.static::uri( $uri, $holder ).'" />';
	}
	
	/**
	 * JS Macro - Generates a js source link
	 * 
	 * @param string 			$uri
	 * @param string 			$holder
	 * @return string
	 */
	protected static function macro_js( $uri, $holder = null )
	{
		return '<script type="text/javascript" src="'.static::uri( $uri, $holder ).'"></script>';
	}
	
	/**
	 * LESS Macro - Generates a less stylesheet link
	 * 
	 * @param string 			$uri
	 * @param string 			$holder
	 * @return string
	 */
	protected static function macro_less( $uri, $holder = null )
	{
		return '<link type="text/css" rel="stylesheet/less" href="'.static::uri( $uri, $holder ).'" />';
	}
	
	/**
	 * IMG Macro - Generates a less stylesheet link
	 * 
	 * @param string 			$uri
	 * @param string 			$holder
	 * @return string
	 */
	protected static function macro_img( $uri, $holder = null )
	{
		return '<img src="'.static::uri( $uri, $holder ).'" />';
	}
	
	/**
	 * Default Macro - Simply returns the given string
	 * 
	 * @param string 			$string
	 * @return string
	 */
	protected static function macro__( $string )
	{
		return $string;
	}
	
	/**
	 * OG Macro - Generates open grapth tags
	 *
	 *     // <meta property="og:type" content="video" />
	 *     CCAsset::og( 'type', 'video' );
	 *     
	 *     // <meta property="og:type" content="video" />
	 *     // <meta property="og:res" content="1080p" />
	 *     CCAsset::og( array( 'type' => 'video', 'res' => '1080p' ));
	 * 
	 * @param array|string 		$tags
	 * @param string 			$content
	 * @return string
	 */
	protected static function macro_og( $tags, $content = null ) 
	{
		if ( !is_array( $tags ) )
		{
			$tags = array( $tags => $content );
		}
		
		$buffer = "";
		
		foreach( $tags as $key => $content )
		{
			$buffer .= '<meta property="og:'.$key.'" content="'.$content.'" />';
		}
		
		return $buffer;
	}
	
	/**
	 * Register an assets macro 
	 * 
	 * @param string					$key
	 * @param callback|string		$callback
	 * @return void
	 */
	public static function macro( $key, $callback )
	{	
		static::$_macros[$key] = $callback; 
	}
	
	/**
	 * Check for macros and execute them
	 *
	 * @param string 	$name
	 * @param array 		$arguments
	 * @return mixed
	 */
	public static function __callStatic( $name, $arguments )
	{
		if ( !array_key_exists( $name, static::$_macros ) )
		{
			// we always check if the global class has that marco this way we
			// can easly extend CCAsset default macros.
			if ( !method_exists( '\\CCAsset', 'macro_'.$name ) )
			{
				throw new \BadMethodCallException( "CCAsset::".$name." - No method or macro found." );
			}
			
			return call_user_func_array( array( '\\CCAsset', 'macro_'.$name ), $arguments );
		}
		
		// if we have a string handle the macro as replacement pattern.
		if ( is_string( static::$_macros[$name] ) )
		{
			// in this case argument 1 is going to be the uri 
			// and argument 2 the asset holder
			list( $uri, $holder ) = $arguments;
			
			return str_replace( ':uri', static::uri( $uri, $holder ), static::$_macros[$name] );
		}
		
		return call_user_func_array( static::$_macros[$name], $arguments );
	}

	/** 
	 * Get the uri to an asset
	 *
	 *     // /assets/images/something.jpg
	 *     CCAsset::uri( 'images/something.jpg' );
	 *     
	 *     // /assets/MyTheme/images/something.jpg
	 *     CCAsset::uri( 'images/logo.png', 'theme' );
	 *     
	 *     // will not modify the given url
	 *     CCAsset::uri( 'http://example.com/images/logo.jpg' );
	 * 
	 * @param string		$uri
	 * @param string		$name 
	 * @return string
	 */
	public static function uri( $uri, $name = null ) 
	{
		// when the uri conains a protocol we just return 
		// the given uri.
		if ( strpos( $uri, '://' ) === false ) 
		{	
			// If the path is relative and not absolute
			// we prefix the holder path.
			if ( substr( $uri, 0, 1 ) != '/' ) 
			{
				$uri = CCUrl::to( static::holder( $name )->path.$uri );
			}
			// When the given uri is absolute we remove the first
			// slash and let CCUrl do the job.
			else 
			{
				$uri = CCUrl::to( substr( $uri, 1 ) );
			}
		}
		
		return $uri;
	}

	/**
	 * Get an asset holder instance.
	 *
	 *     $holder = CCAsset::holder();
	 *     $holder = CCAsset::holder( 'footer' );
	 *
	 * @param string			$name
	 * @return CCAsset
	 */
	public static function holder( $name = null ) 
	{	
		if ( !isset( $name ) ) 
		{
			$name = static::$_default;
		}
		
		if ( !isset( static::$instances[$name] ) ) 
		{
			 static::$instances[$name] = new CCAsset_Holder();
		}
		
		return static::$instances[$name];
	}
	
	/**
	 * Get all assets of a specific type in a holder and run them 
	 * trough the maching macros.
	 *
	 *     CCAsset::code( 'css' );
	 *     CCAsset::code( 'js', 'footer' ); 
	 *
	 * @param string		$type
	 * @param string		$name
	 */
	public static function code( $type, $name = null ) 
	{
		$buffer = "";
		
		foreach( static::holder( $name )->get( $type ) as $item )
		{
			$buffer .= call_user_func( array( '\\CCAsset', $type ), $item, $name );
		}
		
		return $buffer;
	}
	
	/** 
	 * Add an asset to the holder. Basically this method checks the 
	 * file extension to sort them and generate the correct code using the macros.
	 *
	 *     CCAsset::add( 'jquery.js' );
	 *     CCAsset::add( 'style.css' );
	 *     CCAsset::add( '<script>document.write( "Hello World" );</script>', 'footer' );
	 *
	 * @param string			$item
	 * @return void
	 */
	public static function add( $item, $name = null ) 
	{
		return static::holder( $name )->add( $item );
	}
	
	/**
	 * Get all assets by a specific macro / type
	 *
	 *     CCAsset::get();
	 *     CCAsset::get( 'css', 'theme' );
	 *
	 * @param string			$type		By passing null all items from all types are returned
	 * @return array
	 */
	public static function get( $type = null, $name = null ) 
	{
		return static::holder( $name )->get( $type );
	}
	
	/**
	 * Clear the asset holder, deletes all contained assets 
	 * of a special type or if $type is null everything.
	 *
	 *     CCAsset::clear();
	 *     CCAsset::clear( 'css' );
	 *     CCAsset::clear( 'js', 'footer' );
	 * 
	 * @param string 		$type
	 * @return void
	 */
	public static function clear( $type, $name = null ) 
	{
		return static::holder( $name )->clear( $type );
	}	
}