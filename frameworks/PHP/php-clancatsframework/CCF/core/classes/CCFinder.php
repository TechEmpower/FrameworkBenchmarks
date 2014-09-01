<?php
/**
 * ClanCats Finder 
 * The CCFinder handles class loading and binding, autoloading etc.
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCFinder 
{	
	/**
	 * The mapped classes
	 *
	 * @var array
	 */
	public static $classes = array();
	
	/** 
	 * The mapped aliases
	 * 
	 * @var array
	 */
	public static $aliases = array();
	
	/** 
	 * The mapped shadows
	 * 
	 * @var array
	 */
	public static $shadows = array();
	
	/**
	 * The mapped namespaces
	 *
	 * @var array
	 */
	public static $namespaces = array();
	
	/**
	 * The mapped bundles
	 *
	 * @var array
	 */
	public static $bundles = array();
	
	/**
	 * Register the autoloader
	 *
	 * @return  void
	 */
	public static function register() 
	{
		spl_autoload_register( array( '\\CCFinder', 'find' ) );
	}
	
	/**
	 * Add a bundle 
	 * A bundle is a CCF style package with classes, controllers, views etc.
	 *
	 * @param string|array 	$name
	 * @param path 			$path
	 * @return void
	 */
	public static function bundle( $name, $path = null ) 
	{
		static::$bundles[$name] = $path;
		static::$namespaces[$name] = $path.CCDIR_CLASS;
	}
	
	/**
	 * Add one or more maps
	 * A map is simply a class namepsace 
	 *
	 * @param string|array 	$name
	 * @param path 			$path
	 * @return void
	 */
	public static function map( $name, $path = null ) 
	{
		if ( is_array( $name ) ) 
		{
			static::$namespaces = array_merge( static::$namespaces, $name ); return;
		}
		static::$namespaces[$name] = $path;
	}
	
	/**
	 * Add a shadow class 
	 * A shadow class is a global class and gets liftet to the global namespace.
	 * 
	 * \Some\Longer\Namespace\Foo::bar() -> Foo::bar()
	 *
	 * exmpale:
	 *     CCFinder::shadow( 'Foo', 'Some\Longer\Namespace', 'myclasses/Foo.php' );
	 *
	 * @param string			$name		The shadow
	 * @param string			$namespace	The real class namespace
	 * @param string 		$path		The path of the real class
	 * @return void
	 */
	public static function shadow( $name, $namespace, $path = null ) 
	{
		static::$shadows[$name] = $class = $namespace."\\".$name;
		
		if ( !is_null( $path ) )
		{
			static::bind( $name, $class );
		}
	}
	
	/**
	 * Add one or more aliases
	 * An alias can overwrite an shadow. This way we can extend other classes.
	 *
	 * example:
	 *     CCFinder::alias( 'Foo', '/path/to/my/Foo.php' );
	 *
	 * @param string 	$name
	 * @param path 		$path
	 * @return void
	 */
	public static function alias( $name, $path = null ) 
	{
		if ( is_array( $name ) ) 
		{
			static::$aliases = array_merge( static::$aliases, $name ); return;
		}
		static::$aliases[$name] = $path;
	}
	
	/**
	 * Add one or more class to the autoloader
	 *
	 * @param string 	$name
	 * @param path 		$path
	 * @return void
	 */
	public static function bind( $name, $path = null ) 
	{
		if ( is_array( $name ) ) 
		{
			static::$classes = array_merge( static::$classes, $name ); return;
		}
		static::$classes[$name] = $path;
	}
		
	
	/** 
	 * This simply adds some classes with a prefix 
	 * 
	 * @param string 	$name
	 * @param path 		$path
	 * @return void
	 */
	public static function package( $dir, $classes ) 
	{
		foreach( $classes as $name => $path ) 
		{
			static::$classes[$name] = $dir.$path;
		}
	}
	
	
	/** 
	 * This simply adds some shadows with a prefix 
	 * 
	 * @param string 	$name
	 * @param path 		$path
	 * @return void
	 */
	public static function shadow_package( $dir, $namespace, $shadows ) 
	{
		foreach( $shadows as $name => $path ) 
		{
			static::$shadows[$name] = $class = $namespace."\\".$name;
			static::$classes[$class] = $dir.$path;
		}
	}
	
	/**
	 * Autoloading handler
	 *
	 * @param string 	$class
	 * @return bool
	 */
	public static function find( $class ) 
	{	
		if ( class_exists( $class, false ) )
		{
			return;
		}

		// class with or without namespace?
		if ( strpos( $class , '\\' ) !== false ) 
		{
			/*
			 * alias map
			 */
			if ( array_key_exists( $class, static::$aliases ) ) 
			{
				require static::$aliases[$class];
			}
			/* 
			 * normal map
			 */
			elseif ( array_key_exists( $class, static::$classes ) ) 
			{
				require static::$classes[$class];
			}
			/*
			 * try your luck without the map
			 */
			else 
			{	
				$namespace = substr( $class, 0, strrpos( $class, "\\" ) );
				$class_name = substr( $class, strrpos( $class, "\\" )+1 );
				
				if ( !array_key_exists( $namespace, static::$namespaces ) ) 
				{
					return false;
				}
				
				$path = static::$namespaces[$namespace].str_replace( '_', '/', $class_name ).EXT;
				
				if ( !file_exists( $path ) ) 
				{
					return false;
				}
				
				require $path;
			}
			
			/*
			 * check if we need to create a shadow aka an alias
			 */
			if ( in_array( $class, static::$shadows ) )
			{
				$shadow = array_search( $class, static::$shadows );
				
				if ( !class_exists( $shadow, false ) && !array_key_exists( $shadow, static::$aliases ) )
				{
					class_alias( $class, $shadow ); 
				}
			}
		}
		else 
		{
			/*
			 * alias map
			 */
			if ( array_key_exists( $class, static::$aliases ) ) 
			{
				require static::$aliases[$class];
			}
			/*
			 * check shadows
			 */
			if ( array_key_exists( $class, static::$shadows ) )
			{
				return static::find( static::$shadows[$class] );
			}
			/* 
			 * normal map
			 */
			elseif ( array_key_exists( $class, static::$classes ) ) 
			{
				require static::$classes[$class];
			}
			/*
			 * try your luck without the map
			 */
			else 
			{
				$path = APPPATH.CCDIR_CLASS.str_replace( '_', '/', $class ).EXT;
				
				if ( !file_exists( $path ) ) 
				{
					return false;
				}
				
				require $path ;
			}
		}
		
		/*
		 * run the static init if possible
		 */
		if ( method_exists( $class, '_init' ) ) 
		{
			$class::_init();
		}
		
		return true;
	}
}