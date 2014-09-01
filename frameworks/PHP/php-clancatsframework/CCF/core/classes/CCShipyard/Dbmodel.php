<?php namespace Core;
/**
 * ClanCats Shipyard Builder for DB models
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCShipyard_Dbmodel implements CCShipyard_Interface
{	
	protected $name = null;

	protected $table = null;
	
	protected $timestamps = false;

	/**
	 * Start new class builder
	 *
	 * @param string 			$name
	 * @param string 			$table
	 *
	 * @return void
	 */
	public function create( $name, $table )
	{
		$this->name = $name;
		$this->table = $table;
	}
	
	/**
	 * Set automatic timestamps
	 *
	 * @param bool		$set
	 * @return void
	 */
	public function timestamps( $set = true )
	{
		$this->timestamps = (bool) $set;
	}

	/**
	 * Get the current builder output
	 *
	 * @return string
	 */
	public function output() 
	{
		$shema = \DB::fetch( "DESCRIBE {$this->table};", array(), null, 'assoc' );
		
		if ( empty( $shema ) )
		{
			throw new CCException( 'CCShipyard - The given database table is invalid or does not exist.' );
		}
		
		$class = \CCShipyard::create( 'class', $this->name, "\\DB\\Model" );
		
		$class->add( 'property', '_table', 'protected static', $this->table, 'The database table name' );
		
		// timestamps
		if ( $this->timestamps )
		{
			$class->add( 'line', 2 );
			$class->add( 'property', '_timestamps', 'protected static', true, 'Allow automatic timestamps' );
		}
		
		// shema
		$class->add( 'line', 2 );
		
		// define the internal types
		$internal_types = array( 'bool', 'int', 'float', 'double', 'string' );
		
		$match_types = array(
			
			// int
			'tinyint' => 'int',
			
			// string
			'text' => 'string',
			'varchar' => 'string',
		);
		
		foreach( $shema as $item ) 
		{	
			$default = $item['Default'];
			$field = $item['Field'];
			
			if ( empty( $default ) ) 
			{	
				$type = \CCStr::cut( $item['Type'], '(' );
				
				if ( array_key_exists( $type, $match_types ) )
				{
					$type = $match_types[$type];
				}
				elseif ( !in_array( $type, $internal_types ) )
				{
					$type = null;
				}
				
				// The primary key should not contain a default value
				if ( $item['Key'] == 'PRI' ) 
				{
					$type = null;
				}
				// specialcase tinyint 1 assumed as boolean
				elseif ( $item['Type'] == 'tinyint(1)' )
				{
					$type = 'bool';
				}
				
				if ( $type !== null )
				{
					settype( $default, $type );
				}
			}
			
			$buffer = "\t'$field'";
			
			if ( !is_null( $type ) )
			{
				$buffer .= " => array( '".$type."'";
				
				if ( !is_null( $default ) )
				{
					$buffer .= ", ".var_export( $default, true )." )";
				}
			}
			
			$props[] = $buffer;
		}
		
		$class->add( 'property', '_defaults', 'protected static', "array(\n".implode( ",\n", $props )."\n)", 'The '.$class.' default properties', false );
		
		return $class->output();
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