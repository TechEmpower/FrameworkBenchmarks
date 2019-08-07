<?php
namespace controllers;

/**
 * Plaintext controller.
 **/
class Plaintext extends \Ubiquity\controllers\Controller {
	
	public function initialize(){
		\Ubiquity\utils\http\UResponse::setContentType('text/plain; charset=utf-8');
	}
	public function index() {
		echo 'Hello, World!';
	}

}
