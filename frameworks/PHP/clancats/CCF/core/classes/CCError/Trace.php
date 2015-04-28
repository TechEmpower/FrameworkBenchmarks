<?php namespace Core;
/**
 * Trace item
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 * 
 */
class CCError_Trace
{
	/**
	 * file
	 *
	 * @var string
	 */
	public $file = null;
	
	/**
	 * line
	 *
	 * @var string
	 */
	public $line = null;
	
	/**
	 * function
	 *
	 * @var string
	 */
	public $function = null;
	
	/**
	 * class
	 *
	 * @var string
	 */
	public $class = null;
	
	/**
	 * The file 
	 *
	 * @var string
	 */
	public $type = null;
	
	/**
	 * The file 
	 *
	 * @var string
	 */
	public $args = null;
	
	/**
	 * The relevant source code  
	 *
	 * @var string
	 */
	public $source = null;
	
	/**
	 * The file splitted in lines of the reflected calss
	 *
	 * @var array
	 */
	public $reflection_file = null;

	/**
	 * instance contrucor
	 *
	 * @param CCError_Inspector		$inspector
	 * @return void
	 */
	public function __construct( $data )
	{
		foreach( $data as $key => $value )
		{
			$this->$key = $value;
		}
	}
	
	/**
	 * get file path
	 *
	 * @param bool		$cut		Cut 
	 * @return string
	 */
	public function file( $cut = true )
	{
		if ( empty( $this->file ) )
		{
			return null;
		}
		
		if ( $cut )
		{
			return str_replace( CCROOT, '', $this->file );
		}
		
		return $this->file;
	}
	
	/**
	 * get file path
	 *
	 * @return string
	 */
	public function line()
	{
		return $this->line;
	}
	
	/**
	 * get file path
	 *
	 * @return string
	 */
	public function function_name()
	{
		return $this->function;
	}
	
	/**
	 * get file path
	 *
	 * @return string
	 */
	public function class_name()
	{
		return $this->class;
	}
	
	/**
	 * get file path
	 *
	 * @return string
	 */
	public function call_type()
	{
		return $this->type;
	}
	
	/**
	 * get file path
	 *
	 * @return string
	 */
	public function args()
	{
		return $this->args;
	}
	
	/**
	 * create an argument string
	 *
	 * @return string
	 */
	public function args_string()
	{
		if ( !isset( $this->args ) || !is_array( $this->args ) )
		{
			return null;
		}
		
		$args = $this->args;
		
		foreach( $args as $key => $arg )
		{
			if ( is_array( $arg ) )
			{
				$args[$key] = 'Array';
			} 
			else 
			{
				$args[$key] = var_export( $arg, true );
			}
		}
		
		return implode( ', ', $args );
	}
	
	/**
	 * parse the source file
	 *
	 * @return void
	 */
	private function parse_source()
	{
		if ( !$this->file() )
		{
			return null;
		}
		
		// get the file
		$lines = file( $this->file );
		
		/*
		 * parse the relevant source
		 */
		$size = 10;
		
		$start = $this->line - ( $size / 2 );
		$end = $this->line + ( $size / 2 );
		
		// check if start is in the middle of a comment
		while( $start > 0 && substr( trim( $lines[$start-1] ), 0, 1 ) === '*' )
		{
			$start--;
		}
		
		$source = array();
		
		for ( $i=$start-1; $i<$end; $i++ )
		{
			if ( array_key_exists( $i, $lines ) )
			{
				$source[$i] = str_replace( "\t", '    ', $lines[$i] );
			}
		}
		
		$this->source = $source;
	}
	
	/**
	 * get the source code where the trace happend
	 *
	 * @return string
	 */
	public function source(  )
	{	
		if ( is_null( $this->source ) )
		{
			$this->parse_source();
		}
		
		return $this->source;
	}
	
	/**
	 * get the source code where the trace happend
	 *
	 * @return string
	 */
	public function reflection( )
	{	
		if ( is_null( $this->class ) )
		{
			return null;
		}
		
		return new \ReflectionClass( $this->class );
	}
	
	/**
	 * get the source code of the reflected file
	 *
	 * @return string
	 */
	public function reflection_file()
	{	
		if ( is_null( $this->reflection_file ) )
		{
			if ( is_null( $reflection = $this->reflection() ) )
			{
				return null;
			}
			
			$this->reflection_file = file( $reflection->getFileName() );
		}
		
		return $this->reflection_file;
	}
	
	/**
	 * get the source code where the trace happend
	 *
	 * @return string
	 */
	public function reflection_function_info( $function = null )
	{	
		if ( is_null( $lines = $this->reflection_file() ) )
		{
			return null;
		}
		
		if ( is_null( $function ) )
		{
			if ( is_null( $function = $this->function_name() ) )
			{
				return null;
			}
		}
		
		$info = array(
			'message'	=> '',
		);
		
		foreach( $lines as $index => $line )
		{
			// is this the function define
			if ( strpos( $line, 'function '.$function ) !== false )
			{
				// do we have a comment on top
				if ( substr( trim( $lines[$index-1] ), 0, 1 ) === '*' )
				{
					$start = $index-1;
					$comment = array();
					
					// parse the comment
					while( $start > 0 && substr( trim( $lines[$start-1] ), 0, 1 ) === '*' )
					{
						$start--; array_unshift( $comment, substr( trim( trim( $lines[$start] ), "*" ), 1 ) );
					}
					
					// parse quotse
					$in_quote = false;
					
					// we got a comment
					foreach( $comment as $line )
					{
						// add param
						if ( strpos( $line, '@' ) !== false )
						{
							// replace double tabs
							$line = preg_replace( "!\t+!", "\t", $line );
							// make tabs to spaces
							$line = str_replace( "\t", '    ', $line );
							
							$info[trim(substr( $line, 1, strpos( $line, ' ' ) ))][] = trim( substr( $line, strpos( $line, ' ' ) ) );
						}
						// add message
						else
						{
							$line = str_replace( "\t", '    ', $line );
							
							if ( substr( $line, 0, 4 ) == '    ' && !$in_quote )
							{
								$info['message'] .= "<pre>";
								$in_quote = true;
							}
							elseif ( substr( $line, 0, 4 ) != '    ' && $in_quote )
							{
								$info['message'] .= "</pre>";
								$in_quote = false;
							}
							
							if ( $in_quote )
							{
								$line = substr( $line, 4 );
							}
							
							$info['message'] .= $line."\n";
						}
					}
					
					// final trim
					$info['message'] = trim( $info['message'], "\n\r\t" );
					
					// done;
					return $info;
				}
				continue;
			}
		}
	}
}