<?php 
/**
 * ErrorController
 **
 * 
 * @package       CCFApplication
 * @author        Mario Döring <mario@clancats.com>
 * @version       1.0.0
 * @copyright     2010 - 2014 ClanCats GmbH
 *
 */
class ErrorController extends \CCController
{
	/**
	 * Not found 404
	 */
	public function action_404() 
	{
		return CCResponse::create( CCView::create( 'Core::CCF/404' )->render(), 404 );
	}
	
	/**
	 * Server error 500
	 */
	public function action_500() 
	{
		return CCResponse::create( CCView::create( 'Core::CCF/500' )->render(), 500 );
	}
}
