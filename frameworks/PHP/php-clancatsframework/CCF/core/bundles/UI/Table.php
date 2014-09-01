<?php namespace UI;
/**
 * Uiify Table Generator
 *
 * @package 		Uiify
 * @author     	Mario Döring <mario@clancats.com>
 * @version 		0.1
 * @copyright 	2013 ClanCats GmbH
 *
 */
class Table {
	
	/**
	 * get an table instance
	 */
	public static function create( $attr = array() ) {
		return new static( $attr );	
	}
	
	/**
	 * creates an cell
	 */
	public static function cell( $value, $attr = array(), $ele = 'td' ) {
		return HTML::tag( $ele, $value, $attr );
	}
	
	/*
	 * The table element
	 */
	public $element = null;
	
	/*
	 * the table header
	 */
	public $header = array();
	
	/*
	 * data holder
	 */
	public $data = array();
	
	/**
	 * generate
	 */
	public function __construct( $attr = array() ) {
		$this->element = HTML::create( 'table', $attr );
	}
	
	/**
	 * add a new row
	 */
	public function row( $data = array(), $attr = array() ) {
		$this->data[] = $this->_repair_row( $data, $attr ); return $this;
	}
	
	/**
	 * set the header
	 */
	public function header( $data = array(), $attr = array() ) {
		$this->header = $this->_repair_row( $data, $attr ); return $this;
	}
	
	/**
	 * repair a row for rendering
	 */
	private function _repair_row( $data = array(), $attr = array() ) {
		$row = array();
		foreach( $data as $key => $value ) {
			if ( !$value instanceof HTML || ( $value->name != 'td' && $value->name != 'th' ) ) {
				$value = static::cell( (string) $value );
			}
			if ( is_string( $key ) ) {
				$row[$key] = $value;
			} else {
				$row[] = $value;
			}
		}
		return array( $row, $attr );
	}
	
	/**
	 * magic to string
	 */
	public function __toString() {
		return $this->render();
	}
	
	/**
	 * generate the output
	 */
	public function render() {
		
		$buffer = ''; 
		
		if ( !empty( $this->header ) ) {
			
			$buffer .= '<tr'.HTML::attr( $this->header[1] ).'>';
			foreach( $this->header[0] as $head ) {
				$head->name = 'th';
				$buffer .= $head->render();
			}
			$buffer .= '</tr>';
			foreach( $this->data as $row ) {
				$buffer .= '<tr'.HTML::attr( $row[1] ).'>';
				foreach( $this->header[0] as $key => $value ) {
					$buffer .= HTML::tag( 'td', $row[0][$key]->value, $row[0][$key]->attr );
				}
				$buffer .= '</tr>';
			}
		}
		else {
			foreach( $this->data as $row ) {
				$buffer .= '<tr'.HTML::attr( $row[1] ).'>';
				foreach( $row[0] as $value ) {
					$buffer .= $value->render();
				}
				$buffer .= '</tr>';
			}
		}
		
		$this->element->value = $buffer;
		
		return $this->element->render();
	}
}