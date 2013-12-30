<?php

App::uses('AppController', 'Controller');

class PlaintextController extends AppController {

	public function index() {
		$this->autoRender = false;
		header("Content-type: text/plain");
		echo 'Hello, World!';

		/*
		 * Because this test is focused on routing we don't involve the view.
		 *
		 * Normally we would create a template file index.ctp containing "Hello, World!"
		 * in app/View/Plaintext and:
		 *

		$this->autoLayout = false;
		$this->response->type('text');

		 */
	}
}