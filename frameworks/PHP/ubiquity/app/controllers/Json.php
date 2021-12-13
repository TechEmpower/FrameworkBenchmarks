<?php
namespace controllers;

/**
 * Json controller.
 */
class Json extends \Ubiquity\controllers\Controller {

	public function __construct() {}

	public function initialize() {
		\header('Content-Type: application/json');
	}

	public function index() {
		echo \json_encode([
			'message' => 'Hello, World!'
		]);
	}
}
