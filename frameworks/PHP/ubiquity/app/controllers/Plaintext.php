<?php
namespace controllers;

/**
 * Plaintext controller.
 */
class Plaintext extends \Ubiquity\controllers\Controller {

	public function __construct() {}

	public function initialize() {
		\header('Content-Type: text/plain; charset=utf-8');
	}

	public function index() {
		echo 'Hello, World!';
	}
}
