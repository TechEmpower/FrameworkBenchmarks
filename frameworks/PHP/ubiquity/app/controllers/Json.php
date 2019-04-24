<?php
namespace controllers;

use Ubiquity\controllers\Controller;

/**
 * Json controller.
 **/
class Json extends Controller{
	public function initialize() {
		\header( 'Content-Type: application/json;charset=utf-8' );
	}
	public function index(){
		echo \json_encode(['message' => 'Hello, World!']);
	}
}
