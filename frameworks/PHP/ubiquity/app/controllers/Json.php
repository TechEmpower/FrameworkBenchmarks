<?php
namespace controllers;

use Ubiquity\controllers\Controller;
use Ubiquity\utils\http\UResponse;

/**
 * Json controller.
 **/
class Json extends Controller{
	public function initialize() {
		UResponse::setContentType( 'application/json' );
	}
	public function index(){
		echo \json_encode(['message' => 'Hello, World!']);
	}
}
