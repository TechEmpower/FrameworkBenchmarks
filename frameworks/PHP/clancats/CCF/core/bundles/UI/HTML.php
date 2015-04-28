<?php namespace UI;
/**
 * Uiify Base HTML stuff
 *
 * @package 		Uiify
 * @author     	Mario Döring <mario@clancats.com>
 * @version 		0.1
 * @copyright 	2013 ClanCats GmbH
 *
 */
class HTML extends E 
{
	/**
	 * The maker is like a development shortcut to create
	 * html elements on the go
	 *
	 * If param2 is set it wil be used as the content otherwise
	 * the the first parameter will be splittet by the first space.
	 *
	 * @param string			$param
	 * @param string			$param2
	 */
	public static function maker( $param, $param2 = null )
	{
		if ( !is_null( $param2 ) )
		{
			return static::create( $param, $param2 );
		}
		
		$param = explode( ' ', $param );
		
		$element = array_shift( $param );
		
		return static::create( $element, implode( ' ', $param ) );
	}
	
	/**
	 * generates html attribute string
	 *
	 * @param array 	$attr
	 */
	public static function attr( $attr = array() ) {
		$buffer = " ";
		foreach( $attr as $key => $value ) {
		
			switch( $key ) {
		
				case 'class':
				if ( is_array( $value ) ) { $value = implode( ' ', $value ); }
				break;
		
				case 'style':
				if ( is_array( $value ) ) {
					$style = $value; $value = "";
					foreach( $style as $k => $v ) {
						$value .= $k.':'.$v.';';
					}
				}
				break;
			}
		
			$buffer .= $key.'="'.$value.'" ';
		}
		
		return substr( $buffer, 0, -1 );
	}
	
	/**
	 * generates an html tag
	 *
	 * @param string 	$name
	 * @param mixed		$param1
	 * @param mixed		$param2
	 */
	public static function tag( $name, $param1 = null, $param2 = null ) 
	{
		return static::create( $name, $param1, $param2 );
	}
	
	/**
	 * set an data attribute
	 *
	 * @param string	$key
	 * @param string	$value
	 */
	public function data( $key, $value ) {
		return $this->set_attr( 'data-'.$key, $value );
	}
	
	/**
	 * add html class
	 *
	 * @param string 	$class
	 */
	public function add_class( $class ) {
		if ( !isset( $this->attr['class'] ) || !is_array( $this->attr['class'] ) ) {
			$this->_sanitize_class();
		}
		if ( strpos( $class, ' ' ) !== false ) {
			$class = explode( ' ', $class );
		}
		if ( is_string( $class ) ) {
			$class = array( $class );
		}
		foreach ( $class as $c ) {
			$this->attr['class'][] = $c;
		}
		return $this;
	}
	
	/**
	 * remove html class
	 *
	 * @param string 	$class
	 */
	public function remove_class( $class ) {
		if ( !isset( $this->attr['class'] ) || !is_array( $this->attr['class'] ) ) {
			$this->_sanitize_class();
		}
		$this->attr['class'] = array_diff( $this->attr['class'], array( $class ) );
		return $this;
	}
	
	/**
	 * clean the classes attribute
	 */
	private function _sanitize_class() {
		if ( isset( $this->attr['class'] ) && is_string( $this->attr['class'] ) ) {
			$this->attr['class'] = explode( ' ', $this->attr['class'] );
		}
		if ( !isset( $this->attr['class'] ) || !is_array( $this->attr['class'] ) ) {
			$this->attr['class'] = array();
		}
	}
}