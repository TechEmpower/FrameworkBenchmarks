<?php
namespace controllers;

use models\Fortune;

class SwooleFortunes extends \Ubiquity\controllers\Controller {

	public function index() {
		$fortunes = \Ubiquity\orm\DAO::getAll(Fortune::class, '', false);
		$fortunes[] = (new Fortune())->setId(0)->setMessage('Additional fortune added at request time.');
		\usort($fortunes, function ($left, $right) {
			return $left->message <=> $right->message;
		});
		$this->loadView('Fortunes/index.php', [
			'fortunes' => $fortunes
		]);
	}
}

