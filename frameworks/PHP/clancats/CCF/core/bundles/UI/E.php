<?php namespace UI;
/**
 * Uiify Base Element
 *
 * @package 		Uiify
 * @author     	Mario Döring <mario@clancats.com>
 * @version 		0.1
 * @copyright 	2013 ClanCats GmbH
 *
 */
class E {
	
	/**
	 * generates an html tag
	 *
	 * @param string 	$name
	 * @param mixed		$param1
	 * @param mixed		$param2
	 * @return UI\E
	 */
	public static function create( $name, $param1 = null, $param2 = null ) 
	{
		return new static( $name, $param1, $param2 );
	}
	
	/**
	 * Create elemnts by dynamic calls
	 *
	 * @param string 		$name
	 * @param array 			$arguments
	 * @return UI\E
	 */
	public static function __callStatic( $name, $arguments )
	{
		array_unshift( $arguments, $name );
		return forward_static_call_array( array( "UI\\E", 'create' ), $arguments );
	}
	
	/**
	 * generates element attribute string
	 *
	 * @param array 	$attr
	 */
	public static function attr( $attr = array() ) {
		$buffer = " ";
		foreach( $attr as $key => $value ) {	
			$buffer .= $key.'="'.$value.'" ';
		}
	
		return substr( $buffer, 0, -1 );
	}
	
	/*
	 * The element value / content
	 */
	public $value = null;
	
	/*
	 * the elements attributes
	 */
	public $attr = array();
	
	/*
	 * the element name
	 */
	public $name = "";
	
	
	/**
	 * element constructor
	 *
	 * @param string 	$name
	 * @param mixed		$param1
	 * @param mixed		$param2
	 */ 
	public function __construct( $name, $param1 = null, $param2 = null ) {
		/*
		 * Dynamic params
		 */
		if ( is_array( $param1 ) ) {
			$this->attr = $param1;
			if ( is_string( $param2 ) || is_closure( $param2 ) ) {
				$this->value = $param2;
			}
		}
		elseif ( is_string( $param1 ) || is_closure( $param1 ) ) {
			$this->value = $param1;
			if ( is_array( $param2 ) ) {
				$this->attr = $param2;
			}
		}
		
		$this->name = $name;
	}
	
	/**
	 * Set an attribute
	 *
	 * @param string	$attribute
	 * @param mixed		$value
	 * @return $this
	 */
	public function set_attr( $attribute, $value = null ) {
		if ( is_null( $value ) || $value === false ) {
			if ( array_key_exists( $attribute, $this->attr ) ) {
				unset( $this->attr[$attribute] );
			}
		} else {
			if ( $value === true ) {
				$value = $attribute;
			}
			$this->attr[ $attribute ] = $value;
		}
		
		return $this;
	}
	
	/**
	 * magic call to set attributes
	 *
	 * @param string	$method
	 * @param array 	$args
	 * @return $this
	 */
	function __call( $method, $args ) {
		return $this->set_attr( $method, $args[key($args)] );
	}
	
	/**
	 * to string magic
	 *
	 * @return string
	 */
	public function __toString() {
		return $this->render();
	}
	
	/**
	 * render this element
	 *
	 * @return string
	 */
	public function render() {
		// execute callback if we have one first
		if ( is_closure( $this->value ) ) {
			ob_start(); $return = call_user_func( $this->value ); $this->value = ob_get_clean().$return;
		}
		return '<'.$this->name.static::attr( $this->attr ).
			( !is_null( $this->value ) ? '>'.$this->value.'</'.$this->name.'>' : ' />' );
	}
}