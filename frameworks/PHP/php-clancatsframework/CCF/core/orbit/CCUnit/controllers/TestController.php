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
class TestController extends \CCController
{
	/*
	 * turn on the wake response
	 */
	public $wake_has_response = false;
	
	/**
	 * wake event
	 */
	public function wake() 
	{
		if ( $this->wake_has_response )
		{
			return \CCResponse::create( 'Hello Wake' );
		}
	}
	
	/**
	 * index action just deliver Hello World
	 */
	public function action_index()
	{
		return \CCResponse::create( 'Hello World' );        
	}
	
	/**
	 * detail action
	 */
	public function action_detail()
	{
		return \CCResponse::create( 'Test Action' );        
	}
	
	/**
	 * action with param
	 */
	public function action_param( $p1 )
	{
		return \CCResponse::create( 'Test '.$p1 );        
	}
	
	/**
	 * action print 
	 */
	public function action_print( $p1 )
	{
		echo "Test Echo";
	}
	
	/**
	 * action return 
	 */
	public function action_return( $p1 )
	{
		return "Test Return";
	}
}
