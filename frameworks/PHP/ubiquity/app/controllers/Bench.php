<?php
namespace controllers;

use Ubiquity\controllers\Controller;

/**
 * ControllerBase.
 **/
class Bench extends Controller{
	public function index() {
	}

	
	public function plaintext(){
		\header("Content-Type: text/plain");
		echo "Hello, World!";
	}
	public function json(){
		\header( 'Content-Type', 'application/json' );
		echo \json_encode(['message' => 'Hello, World!']);
	}
}

