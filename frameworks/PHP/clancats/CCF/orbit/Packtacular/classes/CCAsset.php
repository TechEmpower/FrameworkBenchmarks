<?php
/**
 * ClanCats Assets wrap
 **
 *
 * @package		Packtacular
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCAsset extends \Core\CCAsset 
{
	/** 
	 * Add something to the packer
	 * This method allows you to add stylesheets, javascript files etc. 
	 * to one single file.
	 *
	 * exmaple:
	 *     CCAsset::pack( 'jquery.js', 'footer', 'core' );
	 *
	 * @param string	|array		$item	An single or an array of assets
	 * @param string 			$name 	The holders name like header / theme / footer
	 * @param string 			$pack	The package like core lib etc.
	 * @return void
	 */
	public static function pack( $item, $name = null, $pack = 'pack' ) 
	{
		if ( !is_array( $item ) ) 
		{
			$items = array( $item => $name );
		}

		$path = PUBLICPATH.static::holder( $name )->path;

		foreach( $items as $file => $holder ) 
		{
			static::holder( $name )->assets['_packtacular'][$pack][CCStr::extension( $item )][] = $path.$file;
		}
	}
	
	/**
	 * Get assets code by type from an holder
	 *
	 * @param string		$extension
	 * @param string		$name
	 */
	public static function code( $extension = null, $name = null ) 
	{
		$assets = static::get( $extension, $name );
		
		// let packtacular handle css and less files
		if ( $extension == 'css' )
		{	
			foreach( static::holder( $name )->assets['_packtacular'] as $key => $pack ) 
			{
				if ( !array_key_exists( 'less', $pack ) ) 
				{
					$pack['less'] = array();
				}
			
				if ( !array_key_exists( 'css', $pack ) ) 
				{
					$pack['css'] = array();
				}
			
				$files = array_merge( $pack['css'], $pack['less'] );
				
				if ( !empty( $files ) )
				{
					CCArr::push( Packtacular::handle( $files, basename( static::holder( $name )->path ).'/'.$name.'/', $key.'_{time}.css' ), $assets );
				}
			}
		}
		// let packtacular handle js files
		elseif ( $extension == 'js' )
		{
			foreach( static::holder( $name )->assets['_packtacular'] as $key => $pack ) 
			{
				if ( !array_key_exists( 'js', $pack ) ) 
				{
					$pack['js'] = array();
				}
				
				if ( !empty( $pack['js'] ) )
				{
					CCArr::push( Packtacular::handle( $pack['js'], basename( static::holder( $name )->path ).'/'.$name.'/', $key.'_{time}.js' ), $assets );
				}
			}
		}
		
		$buffer = "";
		
		foreach( $assets as $item )
		{
			$buffer .= call_user_func( 'CCAsset::'.$extension, $item, $name );
		}
		
		return $buffer;
	}
	
	/**
	 * Get all stylesheets from an instance
	 *
	 * @param string 	$name
	 * @return string
	 */
	public static function styles( $name = null ) 
	{
		$assets = static::holder( $name )->assets['_packtacular'];

		foreach( $assets as $key => $pack ) 
		{
			if ( !array_key_exists( 'less', $pack ) ) 
			{
				$pack['less'] = array();
			}

			if ( !array_key_exists( 'css', $pack ) ) 
			{
				$pack['css'] = array();
			}

			$files = array_merge( $pack['css'], $pack['less'] );
			
			static::add( Packtacular::handle( $files, basename( static::holder( $name )->path ).'/'.$name.'/', $key.'_{time}.css' ), $name );
		}

		return parent::styles( $name );
	}

	/**
	 * Get all scripts from an instance
	 *
	 * @param string 	$name
	 * @return string
	 */
	public static function scripts( $name = null ) 
	{
		$assets = static::holder( $name )->assets['_packtacular'];

		foreach( $assets as $key => $pack ) {

			if ( !array_key_exists( 'js', $pack ) ) 
			{
				$pack['js'] = array();
			}

			static::add( Packtacular::handle( $pack['js'], basename( static::holder( $name )->path ).'/'.$name.'/', $key.'_{time}.js' ), $name );
		}

		return parent::scripts( $name );
	}	

	/*
	 * Content
	 */
	public $assets = array(
		'_packtacular' => array(),
	);
}