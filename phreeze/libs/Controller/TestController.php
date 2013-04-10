<?php
/** @package    HELLO WORLD::Controller */

/** import supporting libraries */
require_once("verysimple/Phreeze/Controller.php");

/**
 *
 * @package HELLO WORLD::Controller
 * @author ClassBuilder
 * @version 1.0
 */
class TestController extends Controller
{
	
	/**
	 * Not used but necessary to implement Controller
	 * @see Controller::Init()
	 */
	protected function Init()
	{
	}
	
	/**
	 * Test route that outputs a simple JSON object
	 */
	public function JSON()
	{
		$arr = array(
		    "message" => "Hello, World!"
		);
		
		$this->RenderJSON($arr);
	}
	
	/**
	 * Test route that connects to the database and outputs
	 * the number of rows specified in the querystring argument "queries"
	 */
	public function DB()
	{
		// Read number of queries to run from URL parameter
		$query_count = RequestUtil::Get('queries',1);

		$arr = array();
		
		for ($i = 0; $i < $query_count; $i++) {
		
			$id = mt_rand(1, 10000);
			
			$world = $this->Phreezer->Get("World",$id);
			
			// convert the Phreezable object into a simple structure for output
			$arr[] = array('id'=>$world->Id,'randomNumber'=>$world->Randomnumber);
		}
		
		$this->RenderJSON($arr);

	}
	
}