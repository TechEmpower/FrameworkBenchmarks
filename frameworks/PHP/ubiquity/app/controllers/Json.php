<?php
namespace controllers;

/**
 * Json controller.
 */
class Json extends \Ubiquity\controllers\Controller {

	public function initialize() {
		\Ubiquity\utils\http\UResponse::setContentType('application/json');
	}

	public function index() {
		echo \json_encode([
			'message' => 'Hello, World!'
		]);
	}
}
