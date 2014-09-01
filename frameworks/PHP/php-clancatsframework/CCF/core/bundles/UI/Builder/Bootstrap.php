<?php namespace UI;
/**
 * Uiify Bootstrap Builder class
 *
 * @package 		Uiify
 * @author     	Mario Döring <mario@clancats.com>
 * @version 		0.1
 * @copyright 	2013 ClanCats GmbH
 *
 */
class Builder_Bootstrap implements Builder_Interface
{
	/**
	 * Build an input form
	 *
	 * @param UI\HTML		$element
	 * @return UI\HTML
	 */
	public function build_form_input( $element )
	{
		return $element->class( 'form-control' );
	}
	
	/**
	 * Build an input form
	 *
	 * @param UI\HTML		$element
	 * @return UI\HTML
	 */
	public function build_form_textarea( $element )
	{
		return $element->class( 'form-control' );
	}
	
	/**
	 * Build an input label
	 *
	 * @param UI\HTML		$element
	 * @return UI\HTML
	 */
	public function build_form_label( $element )
	{
		return $element->class( 'control-label' );
	}
	
	/**
	 * Build an input label
	 *
	 * @param UI\HTML		$element
	 * @return UI\HTML
	 */
	public function build_form_checkbox( $element )
	{
		return HTML::tag( 'div', $element->render() )->class( 'checkbox' );
	}
	
	/**
	 * Build an input label
	 *
	 * @param UI\HTML		$element
	 * @return UI\HTML
	 */
	public function build_form_select( $element )
	{
		return $element->class( 'form-control' );
	}
	
	/**
	 * Build the UI alerts
	 *
	 * @param array 		$alerts
	 * @return UI\HTML
	 */
	public function build_alert( $alerts )
	{	
		return HTML::tag( 'div', function() use( $alerts ) 
		{
			// loop trough all alert types
			foreach( $alerts as $type => $items ) 
			{
				foreach( $items as $alert ) 
				{
					$alert = implode( "<br>\n", $alert );
					
					// close button
					$close = HTML::tag( 'button', '&times;' )
						->add_class( 'close' )
						->type( 'button' )
						->data( 'dismiss', 'alert' );
					
					// alert div
					echo HTML::tag( 'div', $close.$alert )
						->add_class( 'alert fade in' )
						->add_class( 'alert-'.$type );
				}
			}
		})->add_class( 'ui-alert-container' );
	}
}