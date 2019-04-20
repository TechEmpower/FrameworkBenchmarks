<?php
namespace controllers;

use Ubiquity\controllers\Controller;

/**
 * Json controller.
 **/
class Json extends Controller{
	public function initialize() {
		\header( 'Content-Type', 'application/json' );
	}
	public function index(){
		echo \json_encode(['message' => 'Hello, World!']);
	}
}
