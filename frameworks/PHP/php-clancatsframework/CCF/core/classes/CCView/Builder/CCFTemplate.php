<?php namespace Core;
/**
 * CCView CCF template builder
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCView_Builder_CCFTemplate implements CCView_Builder_Interface
{
	/**
	 * The view contents
	 *
	 * @var string
	 */
	protected $content = null;

	/**
	 * Starting commands
	 *
	 * @var array
	 */
	protected $bracket_starting_commands = array(
		'if', 
		'elseif',
		'for',
		'foreach',
		'each',
		'loop',
		'switch',
	);

	/**
	 * Ending commands
	 *
	 * @var array
	 */
	protected $bracket_ending_commands = array(
		'endif',
		'endfor',
		'endforeach',
		'endeach',
		'endloop',
		'break',
		'continue',
		'endswitch',
	);

	/**
	 * Continue commands
	 *
	 * @var array
	 */
	protected $bracket_continue_commands = array(
		'else',
	);

	/**
	 * View builder contructor
	 *
	 * @param string 		$file
	 * @return void
	 */
	public function __construct( $content )
	{
		$this->content = $content;
	}

	/**
	 * Compile method returns the compiled php view file
	 *
	 * @return string
	 */
	public function compile()
	{
		$this->transform( 'echos' );
		$this->transform( 'phptag' );
		$this->transform( 'arrays' );

		//_dd( $this->content );

		return $this->content;
	}

	/**
	 * Execute a transformation
	 *
	 * @param string 		$function
	 * @return void
	 */
	private function transform( $function )
	{
		$this->content = call_user_func( array( $this, 'compile_'.$function ), $this->content );
	}

	/**
	 * Repair an expression
	 */
	public function repair_expression( $exp )
	{
		$commands = explode( ' ', $exp );		

		// filter empty ones
		$commands = array_filter( $commands, function( $value ) 
		{
			return !is_null( $value );
		});

		// bracket starting command
		if ( in_array( $commands[0], $this->bracket_starting_commands ) )
		{
			// each = foreach
			if ( $commands[0] == 'each' )
			{
				$commands[0] = 'foreach';
			}
			// loop special
			elseif ( $commands[0] == 'loop' )
			{
				$commands[0] = 'for';
				$commands[1] = '$i=0;$i<'.$commands[1].';$i++';
			}

			// remove the opening duble point
			end($commands); $key = key($commands);
			if ( substr( $commands[$key], -1 ) == ':' )
			{
				$commands[$key] = substr( $commands[$key], 0, -1 );

				// is it now empty?
				if ( $commands[$key] == ' ' || empty( $commands[$key] ) )
				{
					unset( $commands[$key] );
				}
			}

			// do we have brackets?
			if ( substr( $commands[1], 0, 1 ) != '(' )
			{
				// add starting bracket
				$commands[1] = '( '.$commands[1];

				// add ending bracket
				end($commands); $key = key($commands);
				$commands[$key] .= ' )';
			}

			$commands[] = ':';
		}
		// bracket ending command
		elseif ( count( $commands == 1 ) && in_array( $commands[0], $this->bracket_ending_commands ) )
		{
			// each = foreach
			if ( $commands[0] == 'endeach' )
			{
				$commands[0] = 'endforeach';
			}
			// loop special
			elseif ( $commands[0] == 'endloop' )
			{
				$commands[0] = 'endfor';
			}

			// check for semicolon
			if ( substr( $commands[0], 0, 1 ) != ';' )
			{
				$commands[0] .= ';';
			}
		}
		// bracket continue command
		elseif ( count( $commands == 1 ) && in_array( $commands[0], $this->bracket_continue_commands ) )
		{		
			// remove the opening duble point
			end($commands); $key = key($commands);
			if ( substr( $commands[$key], -1 ) == ':' )
			{
				$commands[$key] = substr( $commands[$key], 0, -1 );

				// is it now empty?
				if ( $commands[$key] == ' ' || empty( $commands[$key] ) )
				{
					unset( $commands[$key] );
				}
			}

			// add the double point
			$commands[] = ':';
		}

		return implode( ' ', $commands );
	}

	/**
	 * Search and replace the echo commands shortcuts
	 *
	 * @return void
	 */
	private function compile_echos( $view )
	{
		return preg_replace_callback('/\{\{(.*?)\}\}/s', function( $match )
		{ 
			$print = trim( $match[1] );

			// add missing semicolons
			if ( substr( $print, -1 ) != ';' )
			{
				$print .= ';';
			}

			return '<?php echo '.$print.' ?>'; 
		}, $view );
	}

	/**
	 * Search and replace for shortcuts of the php tag
	 *
	 * @param string 	$view
	 * @return void
	 */
	private function compile_phptag( $view )
	{
		// I hate this workaround
		$that = $this;

		return preg_replace_callback('/\{\%(.*?)\%\}/s', function( $match ) use( $that )
		{ 
			$expression = trim( $match[1] );

			// repair it 
			$expression = $that->repair_expression( $expression );

			return '<?php '.$expression.' ?>'; 
		}, $view );
	}

	/**
	 * Search and replace vars with . array access
	 *
	 * @param string 	$view
	 * @return void
	 */
	private function compile_arrays( $view )
	{	
		$tokens = token_get_all( $view );

		$tags = array( 0 => '' );
		$tag_index = 0;
		$in_tag = false;

		// parse all php tags out of the view
		foreach ( $tokens as $token ) 
		{
			if ( is_array( $token ) ) 
			{
				if ( $token[0] === T_OPEN_TAG )
				{
					$in_tag = true;
				}

				if ( $in_tag && !in_array( $token[0], array( T_INLINE_HTML ) ) ) 
				{				
					$tags[$tag_index] .= $token[1];
				}

				if ( $token[0] === T_CLOSE_TAG )
				{
					$in_tag = false;
					$tag_index++;
				}
			}
			else 
			{
				if ( $in_tag )
				{
					$tags[$tag_index] .= $token;
				}
			}
		}

		// lets make the tags search keys
		$tags = array_flip( $tags );

		// now search and replace var in the php sections
		foreach( $tags as $search => &$replace )
		{
			$replace = preg_replace_callback('/(\$[a-zA-Z0-9\_\.\-\>\;]+)/s', function( $match )
			{ 
				$var = $match[1];

				if ( strpos( $var, '.' ) !== false )
				{
					$buffer = '';
					$length = strlen( $var );
					$inside_arr = false;

					for( $i=0;$i<$length;$i++ )
					{
						$char = $var[$i];

						if ( $char == '.' && !$inside_arr  )
						{
							$buffer .= "['";
							$inside_arr = true;
						}
						else
						{
							if ( $inside_arr && in_array( $char, array( ';', '-', ',' ) ) )
							{
								$buffer .= "']";
								$inside_arr = false;
							}

							if ( $char == '.' )
							{
								$buffer .= "']['";
								$inside_arr = true;
							}
							else
							{
								$buffer .= $char;
							}
						}
					}

					if ( $inside_arr )
					{
						$buffer .= "']";
					}

					$var = $buffer;
				}

				return $var;

			}, $search );
		}

		return CCStr::replace( $view, $tags );
	}
}