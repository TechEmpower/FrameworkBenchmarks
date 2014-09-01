<?php namespace UI;
/**
 * Uiify Form Generator
 *
 * @package 		Uiify
 * @author     	Mario Döring <mario@clancats.com>
 * @version 		0.1
 * @copyright 	2013 ClanCats GmbH
 *
 */
class Form 
{	
	/**
	 * Current prefix
	 *
	 * @var string
	 */
	private static $id_prefix = null;
	
	/**
	 * Registerd patterns
	 *
	 * @var array[callbacks]
	 */
	private static $macros = array();
	
	/**
	 * Is the builder enabled
	 *
	 * @var bool
	 */
	private static $builder_enabled = false;
	
	/**
	 * Static init
	 * Here we register all inital macros
	 */
	public static function _init()
	{
		static::$builder_enabled = Builder::$config->get( 'form.builder_enabled' );
	}
	
	/**
	 * Create a new static pattern for the form generator.
	 * I found this Macro idea in the wonderfull laravel framework thanks.
	 *
	 * @param string			$key
	 * @param callback		$callback
	 * @return void
	 */
	public static function macro( $key, $callback )
	{
		static::$macros[$key] = $callback;
	}
	
	/**
	 * Open a new form
	 * This will set the current form to this one
	 * 
	 * @param string			$key
	 * @param array 			$attr
	 * @return string
	 */
	public static function start( $key, $attr = array() )
	{
		$attributes = array();
		
		// force the form role
		$attributes['role'] = 'form';
		
		if ( !is_null( $key ) )
		{
			 static::$id_prefix = $attributes['id'] = static::form_id( 'form', $key );
		}
		
		$attributes = array_merge( $attributes, $attr );
		
		return '<form'.HTML::attr( $attributes ).'>';
	}
	
	/**
	 * Closes the form and resest the current form
	 * 
	 * @param string			$key
	 * @param array 			$attr
	 * @return string
	 */
	public static function end()
	{
		static::$id_prefix = null; return '</form>';
	}
	
	/**
	 * Create a new from instance
	 *
	 * @param callback		$callback	
	 * @param string			$key			The form key used for identification.
	 * @param array 			$attr		The form dom attributes.
	 * @return UI\Form
	 */
	public static function capture( $callback = null, $key = null, $attr = null ) 
	{	
		// we got some dynamics in the parameters here so in case
		// of this shift stuff
		if ( is_callable( $attr ) && !is_callable( $callback ) )
		{
			$new_attr = $key;
			$key = $callback;
			$callback = $attr;
			$attr = $new_attr;
		}
		
		$form = new static;
		
		if ( is_null( $callback ) )
		{
			throw new Exception( 'Cannot use capture without a callback or string given.' );
		}
		
		// fix no array given
		if ( !is_array( $attr ) )
		{
			$attr = array();
		} 
		
		return static::start( $key, $attr ).\CCStr::capture( $callback, array( $form ) ).static::end();
	}
	
	/**
	 * Format an id by configartion
	 *
	 * @param string 		$type 	element, form etc..
	 * @param string			$name
	 * @return string
	 */
	public static function form_id( $type, $name )
	{
		return sprintf( Builder::$config->get( 'form.'.$type.'_id_format' ), $name );
	}
	
	/**
	 * Format an id by configartion with the current form prefix
	 *
	 * @param string 		$type 	element, form etc..
	 * @param strgin			$name
	 * @return string
	 */
	public static function build_id( $type, $name )
	{
		if ( !is_null( static::$id_prefix ) )
		{
			return static::$id_prefix.'-'.static::form_id( $type, $name );
		}
		return static::form_id( $type, $name );
	}
	
	/**
	 * Forward intance functions to static using the current instance
	 *
	 * @param string 		$method
	 * @param array 			$args
	 * @return mixed
	 */
	public static function __callStatic( $method, $args ) 
	{
		// take the first argument and add it again as the id
		array_unshift( $args, static::build_id( $method, reset( $args ) ) );
		
		if ( array_key_exists( $method, static::$macros ) )
		{
			// execute the macro
			return call_user_func_array( static::$macros[$method], $args );
		}
		
		if ( method_exists( __CLASS__, 'make_'.$method ) )
		{
			return call_user_func_array( array( __CLASS__, 'make_'.$method ), $args );
		}
		
		throw new Exception( "UI\\Form - Unknown macro '".$method."'." );
	}
	
	/**
	 * Simply forward to call static to allow execution from 
	 * object context
	 *
	 * @param string 		$method
	 * @param array 			$args
	 * @return mixed
	 */
	public function __call( $method, $args )
	{
		return static::__callStatic( $method, $args );
	}
	
	/**
	 * make an input
	 *
	 * @param string		$id			The id that has been generated for us.
	 * @param string 	$key			This is the name 
	 * @param string		$type
	 * @param array 		$attr
	 */
	public static function make_input( $id, $key, $value = null, $type = 'text', $attr = array() ) 
	{
		$element = HTML::tag( 'input', array_merge( array( 
			'id' => $id, 
			'name' => $key, 
			'type' => $type 
		), $attr ));
		
		if ( !is_null( $value ) )
		{
			$element->value( _e( $value ) );
		}
		
		if ( !static::$builder_enabled )
		{
			 return $element;
		}
		
		return Builder::handle( 'form_input', $element );
	}
	
	/**
	 * make a label
	 *
	 * @param string		$id			The id that has been generated for us.
	 * @param string 	$key			This is the name 
	 * @param string		$text
	 * @param array 		$attr
	 */
	public static function make_label( $id, $key, $text = null, $attr = array() ) 
	{
		if ( is_null( $text ) )
		{
			$text = $key;
		}
		
		$element = HTML::tag( 'label', $text, array_merge( array( 
			'id' => $id, 
			'for' => static::build_id( 'input', $key ) 
		), $attr ));
		
		if ( !static::$builder_enabled )
		{
			 return $element;
		}
		
		return Builder::handle( 'form_label', $element );
	}
	
	/**
	 * make a checkbox
	 *
	 * @param string		$id			The id that has been generated for us.
	 * @param string 	$key			This is the name 
	 * @param string		$text
	 * @param bool		$active		Is the checkbox cheked
	 * @param array 		$attr
	 * @return string
	 */
	public static function make_checkbox( $id, $key, $text = '', $active = false, $attr = array() ) 
	{
		$element = HTML::tag( 'input', array_merge( array( 
			'id' => $id,
			'name' => $key, 
			'type' => 'checkbox' 
		), $attr ));
		
		$element->checked( (bool) $active );
		
		$element = HTML::tag( 'label', $element->render().' '.$text );
		
		if ( !static::$builder_enabled )
		{
			 return $element;
		}
		
		return Builder::handle( 'form_checkbox', $element );
	}
	
	/**
	 * generate a textarea
	 *
	 * @param string		$id			The id that has been generated for us.
	 * @param string 	$key			This is the name 
	 * @param string		$value
	 * @param array 		$attr
	 * @return string
	 */
	public static function make_textarea( $id, $key, $value = '', $attr = array() ) 
	{
		$element = HTML::tag( 'textarea', _e( $value ), array_merge( array( 
			'id' => $id, 
			'name' => $key,
		), $attr ));
		
		if ( !static::$builder_enabled )
		{
			 return $element;
		}
		
		return Builder::handle( 'form_textarea', $element );
	}
	
	/**
	 * generate a file input
	 *
	 * @param string		$id			The id that has been generated for us.
	 * @param string 	$key			This is the name
	 * @param array 		$attr
	 * @return string
	 */
	public static function make_file( $id, $key, $attr = array() ) 
	{
		return static::make_input( $id, $key, null, 'file', $attr );
	}

	/**
	 * generate an select
	 *
	 *     Form::select( 'gender', array( 'F', 'M' ), 0 );
	 *
	 *     Form::select( 'gender', array( '1' => 'A', '2' => 'B' ), array( 1,2 ), 2 );
	 *
	 * @param string		$id			The id that has been generated for us.
	 * @param string 	$name		This is the name
	 * @param array		$options
	 * @param array 		$selected
	 * @param int		$size
	 * @return string
	 */
	public static function make_select( $id, $name, array $options, $selected = array(), $size = 1 ) 
	{	
		if ( !is_array( $selected ) ) 
		{
			$selected = array( $selected );
		}
		
		$buffer = "";
		
		foreach( $options as $key => $value )
		{
			$option = HTML::tag( 'option', $value )
				->value( $key );
			
			if ( in_array( $key, $selected ) )
			{
				$option->selected( true );
			}
			
			$buffer .= $option->render();
		}
		
		$element = HTML::tag( 'select', $buffer, array(
			'id' => $id,
			'name' => $name,
			'size' => $size,
		) );
		
		if ( !static::$builder_enabled )
		{
			 return $element;
		}
		
		return Builder::handle( 'form_select', $element );
	}
}