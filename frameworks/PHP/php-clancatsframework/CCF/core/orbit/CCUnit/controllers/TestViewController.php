<?php namespace CCUnit;
/**
 * TestController
 **
 * 
 * @package       CCUnit
 * @author        Mario Döring <mario@clancats.com>
 * @version       1.0.0
 * @copyright     2010 - 2014 ClanCats GmbH
 */
class TestViewController extends \CCViewController
{
	/**
	 * The default theme
	 * if you wish you can render this controller with a special theme
	 *
	 * @var string
	 */
	protected $_theme = 'CCUnit';
	
	/**
	 * The default layout
	 * define another default layout than the one that the theme defines.
	 *
	 * @var string
	 */
	protected $_layout = null;
	
	/**
	 * index action just deliver Index
	 */
	public function action_index()
	{
		echo "Index";     
	}
	
	/**
	 * return action Return an string
	 */
	public function action_return()
	{
		return "Return";     
	}
	
	/**
	 * return a response
	 */
	public function action_response()
	{
		return \CCResponse::create( 'Response' );        
	}
	
	/**
	 * use a view
	 */
	public function action_view()
	{
		$this->view = \CCView::create( 'CCUnit::test', array(
			'foo'	=> 'Bar',
		));     
	}
	
	/**
	 * use a view in modal
	 */
	public function action_modal()
	{
		$this->modal = true;
		return $this->action_view();  
	}
}
