<?php
namespace controllers;

use models\Fortune;
use Ubiquity\orm\DAO;

class WorkerFortunes extends \Ubiquity\controllers\Controller {

	public function initialize() {
		\Ubiquity\utils\http\UResponse::setContentType('text/html','utf-8');
	}
	
	public function index() {
		$fortunes = DAO::getAll(Fortune::class, '', false);
		$fortunes[] = (new Fortune())->setId(0)->setMessage('Additional fortune added at request time.');
		\usort($fortunes, function ($left, $right) {
			return $left->message <=> $right->message;
		});
		$this->loadView('Fortunes/index.php', [
			'fortunes' => $fortunes
		]);
	}
}

