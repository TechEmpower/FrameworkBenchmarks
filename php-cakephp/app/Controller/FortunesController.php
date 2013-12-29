<?php

App::uses('AppController', 'Controller');

class FortunesController extends AppController {

	public function index() {
		// use full view stack as encouraged by test rules
		$this->layout = 'benchmark';
		$this->set('title_for_layout', 'Fortunes');

		$this->loadModel('Fortune');
		$results      = $this->Fortune->find('all');

		// stuffing in the dynamic data
		$results[]['Fortune'] = array(
			'id'      => 0,
			'message' => 'Additional fortune added at request time.'
		);

		// because we are performance concerned we don't use Hash::sort()
		foreach ($results as $result) {
			$fortunes[$result['Fortune']['id']] = $result['Fortune']['message'];
		}
		asort($fortunes);

		$this->set('fortunes', $fortunes);
	}

	public function raw() {
		// use full view stack as encouraged by test rules
		$this->layout = 'benchmark';
		$this->set('title_for_layout', 'Fortunes');

		// using prepared query as encouraged by test rules
		$db      = $this->Fortune->getDataSource();
		$results = $db->fetchAll('SELECT * FROM Fortune');

		// stuffing in the dynamic data
		$results[]['Fortune'] = array(
			'id'      => 0,
			'message' => 'Additional fortune added at request time.'
		);

		// because we are performance concerned we don't use Hash::sort()
		foreach ($results as $result) {
			$fortunes[$result['Fortune']['id']] = $result['Fortune']['message'];
		}
		asort($fortunes);

		$this->set('fortunes', $fortunes);
		$this->render('index');
	}

}
