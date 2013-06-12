<?php
namespace App\Controller;

class Benchmark extends \PHPixie\Controller {

	public function action_json(){
		$this->response->add_header("Content-Type: application/json");
		$this->response-> body = json_encode(array('message' => 'Hello, World!'));
	}
	
	public function action_db() {
		$this->response->add_header("Content-Type: application/json");
		$this->response->body = json_encode($this->fetch_random_world()->as_array());
	}
	
	public function action_queries() {
		$this->response->add_header("Content-Type: application/json");
		$n = (int)$this->request->get('queries', 1);
		if ($n < 1)
			$n = 1;
		if ($n > 500)
			$n = 500;
		$res = array();
		for ($i = 0; $i < $n; $i++)
			$res[] = $this->fetch_random_world()->as_array();
		$this->response->body = json_encode($res);
	}
	
	public function action_fortunes() {
		$fortunes = $this->pixie-> orm->get('fortune')->find_all()->as_array();
		$fortune = $this->pixie->orm->get('fortune');
		$fortune->id = 0;
		$fortune->message = 'Additional fortune added at request time.';
		$fortunes[] = $fortune;
		
		usort($fortunes, function($a, $b) { 
			$am = $a->message;
			$bm = $b->message;
			if ($am==$bm) return 0;
			return ($am<$bm)?-1:1;
		} );
		
		$view = $this->pixie->view('fortunes');
		$view->fortunes = $fortunes;
		$this->response->body = $view->render();
		
	}
	
	public function action_updates(){
		$this->response->add_header("Content-Type: application/json");
		$n = (int)$this->request->get('queries', 1);
		if ($n < 1)
			$n = 1;
		if ($n > 500)
			$n = 500;
		$res = array();
		for ($i = 0; $i < $n; $i++) {
			$world = $this->fetch_random_world();
			$world->randomNumber = rand(1, 10000);
			$world->save();
			$res[]=$world->as_array();
		}
		$this->response->body = json_encode($res);
	}
	
	public function action_plaintext() {
		$this->response-> add_header("Content-Type: application/json");
		$this->response->body = "Hello, World!";
	}
	
	protected function fetch_random_world() {
		return $this->pixie->orm->get('world')
				->where('id', rand(1, 10000))
				->find();
	}
	
	
}