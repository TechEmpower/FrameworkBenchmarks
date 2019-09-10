<?php
namespace controllers;

use models\Fortune;
use Ubiquity\orm\DAO;

class SwooleFortunes extends \Ubiquity\controllers\Controller {

	public function index() {
	    $dbInstance=DAO::pool();
		$fortunes = DAO::getAll(Fortune::class, '', false);
		DAO::freePool($dbInstance);
		$fortunes[] = (new Fortune())->setId(0)->setMessage('Additional fortune added at request time.');
		\usort($fortunes, function ($left, $right) {
			return $left->message <=> $right->message;
		});
		$this->loadView('Fortunes/index.php', [
			'fortunes' => $fortunes
		]);
	}
}

