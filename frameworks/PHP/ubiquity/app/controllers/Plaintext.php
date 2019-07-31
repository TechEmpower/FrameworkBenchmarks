<?php
namespace controllers;

use Ubiquity\controllers\Controller;
use Ubiquity\utils\http\UResponse;

/**
 * Plaintext controller.
 **/
class Plaintext extends Controller{
	
	public function initialize(){
		UResponse::setContentType('text/plain; charset=utf-8');
	}
	public function index() {
		echo 'Hello, World!';
	}

}
