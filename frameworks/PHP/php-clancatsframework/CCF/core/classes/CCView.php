<?php namespace Core;
/**
 * View
 * rendering html pages 
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCView extends CCDataObject 
{	
	/**
	 * global data holder
	 *
	 * @var array
	 */
	public static $_globals = array();
	
	/**
	 * A runtime cache for the view paths
	 *
	 * @var array
	 */
	public static $_view_paths = array();
	
	/**
	 * Share a var global
	 *
	 * @param string 		$key
	 * @param mixed 			$value
	 * @return void
	 */
	public static function share( $key, $value )
	{
		CCArr::set( $key, $value, static::$_globals );
	}
	
	/**
	 * View creator
	 * returns a new view instance
	 *
	 * @param string		$file
	 * @param array 		$data
	 * @param bool		$encode
	 * @return CCView
	 */
	public static function create( $file = null, $data = array(), $encode = false ) 
	{
		return new static( $file, $data, $encode );
	}
	
	/**
	 * check if the view exists
	 *
	 * @param string 	$file
	 * @return bool
	 */
	public static function exists( $file )
	{
		return file_exists( CCPath::get( $file, CCDIR_VIEW, EXT ) ); 
	}
	
	/**
	 * Get the cache path of a view
	 *
	 * @param string			$view
	 * @return string
	 */
	public static function cache_path( $view )
	{
		if ( strpos( $view, '::' ) !== false )
		{
			list( $package, $view ) = explode( '::', $view );
		}
		else 
		{
			$package = 'app';
		}
		
		return \CCStorage::path( 'views/'.$package.'/'.$view.EXT );
	}
	
	/**
	 * view file
	 *
	 * @var string
	 */
	protected $_file = null;
	
	/**
	 * View contructor
	 * assign the view file and inital data
	 *
	 * @param string		$file
	 * @param array 		$data
	 * @param bool		$encode
	 * @return CCView
	 */
	public function __construct( $file, $data = array(), $encode = false )
	{
		if ( !is_null( $file ) )
		{
			$this->_file = $file;
		}
		
		if ( !is_array( $data ) )
		{
			$data = array();
		}
		
		foreach( $data as $key => $value )
		{
			$this->set( $key, $value, $encode );
		}
	}
	
	/**
	 * set or get the current file
	 *
	 * @param string		$file
	 * @return string
	 */
	public function file( $file = null )
	{
		if ( !is_null( $file ) )
		{
			return $this->_file = $file;
		}
		
		return $this->_file;
	}
	
	/**
	 * custom setter with encode ability
	 *
	 * @param string 	$key
	 * @param mixed		$value
	 * @param mixed 		$param
	 * @return void
	 */
	public function set( $key, $value, $param = null ) 
	{
		if ( $param === true )
		{
			$value = CCStr::htmlentities( $value );
		}
		
		return CCArr::set( $key, $value, $this->_data );
	}

	/**
	 * just like set but it can
	 * captures all output in a closure and set that.
	 *
	 * @param string			$key
	 * @param callback		$callback
	 * @return void
	 */
	public function capture( $key, $callback ) 
	{
		return $this->set( $key, CCStr::capture( $callback, $this ) );
	}
	
	/**
	 * just like capture but it appends
	 *
	 * @param string			$key
	 * @param callback		$callback
	 * @return void
	 */
	public function capture_append( $key, $callback ) 
	{
		return $this->set( $key, $this->get( $key, '' ).CCStr::capture( $callback, $this ) );
	}
	
	/**
	 * magic to string method
	 * this way a view can be directly printed
	 * 
	 * @return string
	 */
	public function __toString() 
	{
		return $this->render();
	}
	
	/**
	 * Get the path to real view file
	 *
	 * This function will also build the cache view file if needed.
	 *
	 * @param string 		$path
	 * @return string 
	 */
	protected function view_path( $view )
	{
		if ( isset( static::$_view_paths[$view] ) )
		{
			return static::$_view_paths[$view];
		}
		
		// view is empty?
		if ( is_null( $view ) )
		{
			throw new CCException( "CCView - cannot render view without a view file." );
		}
		
		// generate the views path
		$path = CCPath::get( $view, CCDIR_VIEW, EXT );
		
		if ( !file_exists( $path ) ) 
		{
			throw new CCException( "CCView - could not find view: ".$view." at: {$path}." );
		}
		
		// does the view implement a view builder?
		if ( strpos( $view, '.' ) !== false )
		{
			$cache = static::cache_path( $view );
			
			// does the cache not exits or is out of date
			if ( !file_exists( $cache ) || filemtime( $cache ) < filemtime( $path ) )
			{
				list( $view_name, $builder ) = explode( '.', $view );
				$this->build_cache( $cache, $builder, $path );
			}
			
			$path = $cache;
		}
		
		return static::$_view_paths[$view] = $path;
	}
	
	/**
	 * Build a view cache file
	 *
	 * @param string 		$path 
	 * @param string 		$builder 
	 * @param string 
	 *
	 * @return void
	 */
	protected function build_cache( $path, $builder, $view_path )
	{
		CCFile::write( $path, CCView_Builder::compile( $builder, $view_path ) );
	}
	
	/**
	 * Render the view and return the output.
	 *
	 * @param string		$file
	 * @return string
	 */
	public function render( $file = null ) 
	{
		if ( !is_null( $file ) )
		{
			$this->file( $file );
		}
		
		$path = $this->view_path( $this->file() );
		
		// extract the view data
		extract( $this->_data );
			
		// extract globals if they exists
		if ( !empty( static::$_globals ) ) 
		{
			extract( static::$_globals, EXTR_PREFIX_SAME, 'global' );
		}
			
		ob_start();
		
		require( $path );

		return ob_get_clean();
	}
}