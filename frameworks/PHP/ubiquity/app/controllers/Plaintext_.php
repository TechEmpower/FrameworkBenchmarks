<?php
namespace controllers;

/**
 * Plaintext controller.
 */
class Plaintext_ extends \Ubiquity\controllers\Controller {

	public function __construct() {}

	public function initialize() {
		\Ubiquity\utils\http\UResponse::setContentType('text/plain', 'utf-8');
	}

	public function index() {
		echo 'Hello, World!';
	}
}
