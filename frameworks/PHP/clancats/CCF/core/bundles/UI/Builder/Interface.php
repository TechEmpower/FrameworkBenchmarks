<?php namespace UI;
/**
 * Uiify Builder Interface
 *
 * @package 		Uiify
 * @author     	Mario Döring <mario@clancats.com>
 * @version 		1.0
 * @copyright 	2013 ClanCats GmbH
 *
 */
interface Builder_Interface
{
	/**
	 * Build an input form
	 *
	 * @param UI\HTML		$element
	 * @return UI\HTML
	 */
	public function build_form_input( $element );
	
	/**
	 * Build an input label
	 *
	 * @param UI\HTML		$element
	 * @return UI\HTML
	 */
	public function build_form_label( $element );
	
	/**
	 * Build an input label
	 *
	 * @param UI\HTML		$element
	 * @return UI\HTML
	 */
	public function build_form_checkbox( $element );
	
	/**
	 * Build the UI alerts
	 *
	 * @param array 		$alerts
	 * @return UI\HTML
	 */
	public function build_alert( $alerts );
}