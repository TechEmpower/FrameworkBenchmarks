<?php

App::uses('AppController', 'Controller');

class PlaintextController extends AppController {

	public function index() {
		$this->autoRender = false;
		$this->response->type('text');
		$this->response->body('Hello, World!');

		return $this->response;

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
