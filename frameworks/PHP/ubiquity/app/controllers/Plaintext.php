<?php
namespace controllers;

use Ubiquity\controllers\Controller;

/**
 * Plaintext controller.
 **/
class Plaintext extends Controller{
	
	public function initialize(){
		\header("Content-Type: text/plain");
	}
	public function index() {
		echo "Hello, World!";
	}

}
