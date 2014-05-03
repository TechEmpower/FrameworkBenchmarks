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
		require_once("Model/World.php");
		
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
	
	/**
	 * Output the Fortunes test template
	 */
	public function Fortunes()
	{
		require_once("Model/Fortune.php");
		require_once("verysimple/Phreeze/PHPRenderEngine.php");
		
		// charset must be set to UTF8 to support multi-byte chars
		$this->Phreezer->DataAdapter->ConnectionSetting->Charset = "utf8";
		
		// obtain fortunes without using 'order by'
		$fortunes = $this->Phreezer->Query('Fortune')->ToObjectArray();
		
		// dynamically add a new, non-persisted Fortune object
		$newFortune = new Fortune($this->Phreezer);
		$newFortune->Id = 0;
		$newFortune->Message = 'Additional fortune added at request time.';
		$fortunes[] = $newFortune;
		
		// sort (will use Fortune->ToString)
		Phreezer::Sort($fortunes);
		
		// Render using a template
		$this->RenderEngine = new PHPRenderEngine('templates');
		$this->Assign('fortunes',$fortunes);
		$this->Render('TestFortunes.php');
	}
	
	/**
	 * Test for performing updates
	 */
	public function Updates()
	{
		require_once("Model/World.php");
		
		// Read number of queries to run from URL parameter
		$query_count = RequestUtil::Get('queries',1);
		
		$arr = array();
		
		for ($i = 0; $i < $query_count; $i++) {
		
			$id = mt_rand(1, 10000);
				
			$world = $this->Phreezer->Get("World",$id);
			
			// update the random number and persist the record
			$world->Randomnumber = mt_rand(1, 10000);
			$world->Save();
				
			// convert the Phreezable object into a simple structure for output
			$arr[] = array('id'=>$world->Id,'randomNumber'=>$world->Randomnumber);
		}
		
		$this->RenderJSON($arr);
	}
	
	/**
	 * Test for outputting plaintext
	 */
	public function PlainText()
	{
		header('Content-type: text/plain');
		echo 'Hello, World!';
	}
	
}