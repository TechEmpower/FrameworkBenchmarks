<?php namespace Core;
/**
 * ClanCats Shipyard Builder
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCShipyard_Class implements CCShipyard_Interface
{	
	protected $name = null;
	
	protected $extends = null;
	
	protected $implements = array();
	
	/**
	 * The builder output
	 *
	 * @var string
	 */
	protected $output;
	
	
	/**
	 * Start new class builder
	 *
	 * @param string 			$name
	 * @param string 			$extends
	 * @param string|array 		$implements
	 *
	 * @return void
	 */
	public function create( $name, $extends = null, $implements = null )
	{
		$this->name = $name;
		$this->extends = $extends;
		
		if ( !is_array( $implements ) && !is_null( $implements ) )
		{
			$implements = array( $implements );
		}
		
		$this->implements = $implements;
	}
	
	/**
	 * Add a component to the class
	 *
	 * @param mixed ...
	 * @return void
	 */
	public function add()
	{
		$args = func_get_args();
		
		$component = array_shift( $args );
		
		$method = 'build_'.strtolower( $component );
		
		if ( !method_exists( $this, $method ) )
		{
			throw new CCException( 'CCShipyard_Class invalid component builder "'.$component.'".' );
		}
		
		$this->output .= call_user_func_array( array( $this, $method ), $args );
	}
	
	/**
	 * Builds a new class property
	 *
	 * @param string 		$name
	 * @param string 		$context
	 * @param mixed			$default
	 * @param string			$comment
	 * @param bool			$export_default
	 */
	public function build_property( $name, $context = null, $default = null, $comment = null, $export_default = true )
	{	
		if ( is_null( $context ) )
		{
			$context = 'public';
		}
		
		if ( is_null( $comment ) )
		{
			$comment = ucfirst( $name );
		}
		
		if ( $default !== null )
		{
			$type = gettype( $default );
			
			// integer to int
			if ( $type === 'integer' )
			{
				$type = 'int';
			} 
			
			// boolean to bool
			if ( $type === 'boolean' )
			{
				$type = 'bool';
			} 
			
			$comment .= "\n\n@var ".$type;
		}
		
		$forge = new \CCForge_Php;
		
		return $forge->property( $context.' $'.$name, $default, $comment, $export_default );
	}
	
	/**
	 * Builds a new class property
	 *
	 * @param string 		$name
	 * @param string 		$context
	 * @param mixed			$content
	 * @param string			$comment
	 */
	public function build_function( $name, $context = null, $content = null, $comment = null )
	{	
		if ( is_null( $context ) )
		{
			$context = 'public';
		}
		
		if ( strpos( $name, '(' ) === false && strpos( $name, '(' ) === false )
		{
			$name .= '()';
		}
		
		if ( is_null( $comment ) )
		{
			$comment = ucfirst( $name )." function\n\nreturn void";
		}
		
		$forge = new \CCForge_Php;
		return $forge->closure( $context.' function '.$name, $content, $comment );
	}
	
	/**
	 * Create new lines
	 *
	 * @param int 		$nums
	 */
	public function build_line( $nums = 1 )
	{	
		return \CCForge_Php::make( 'line', $nums );
	}
	
	/**
	 * generates an file header string
	 *
	 * @param string		$title
	 * @param array 		$data
	 * @return string
	 */
	protected function file_header( $title, $data = array() ) 
	{	
		$conf = \CCConfig::create( 'shipyard' );
		$data = CCDataObject::assign( $data );
		
		// Build the authors string
		$authors = $data->get( 'authors', $conf->get( 'defaults.authors' ) );		
		
		if ( is_array( $authors ) )
		{
			foreach( $authors as $person ) 
			{
				$author_str .= $person['name']." ";
				if ( array_key_exists( 'email', $person ) ) 
				{
					$author_str .= "<".$person['email'].">";
				}
				$author_str .= ", ";
			}
			$author_str = substr( $author_str, 0, -2 );
		}
		
		// Finish comment heeader
		return "$title\n".
			"*\n".
			"\n".
			"@package       ".$data->get( 'package', $conf->get( 'defaults.package' ) )."\n".
			"@author        ".$author_str."\n".
			"@version       ".$data->get( 'version', $conf->get( 'defaults.version' ) )."\n".
			"@copyright     ".$data->get( 'copyright', $conf->get( 'defaults.copyright' ) )."\n";
	}
	
	/**
	 * Get the current builder output
	 *
	 * @return string
	 */
	public function output() 
	{
		$namespace = null;
		$class = $this->name;
		
		extract( \CCConfig::create( 'shipyard' )->defaults );
		
		// get namespace from the param
		if ( strpos( $class, '::' ) !== false )
		{
			list( $namespace, $class ) = explode( '::', $this->name );
			
			// try to get the ship from namespace
			if ( $ship = \CCOrbit::ship_by_namespace( $namespace ) )
			{
				$package = $ship->name;
				$version = $ship->version;
				$authors = $ship->authors;
			}
		}
		
		// create forge instance
		$forge = new \CCForge_Php( $namespace );
		
		// add header
		$forge->comment( $this->file_header( $class, array(
			'package'	=> $package,
			'authors'	=> $authors,
			'version'	=> $version,
			'copyright'	=> $copyright
		)));
		
		$class_string = 'class '.$class;
		
		// superclasses
		if ( $this->extends )
		{
			$class_string .= ' extends '.$this->extends;
		}
		
		// interface implementations
		if ( !empty( $this->implements ) )
		{
			$class_string .= ' implements '.implode( ', ', $this->implements );
		}
		
		// create the closure
		$forge->closure( $class_string, $this->output );
		
		return $forge->buffer;
	}
	
	/**
	 * Write the file down
	 *
	 * @return bool
	 */
	public function write()
	{
		// resolve the path
		if ( !$path = \CCPath::classes( str_replace( '_', '/', $this->name ), EXT ) )
		{
			throw new CCException( 'Could not resolve the class path. Check if the namespace is registered.' );
		}
	}
}