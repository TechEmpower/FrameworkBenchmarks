<?php
namespace controllers;

use models\Fortune;

class WorkerFortunes extends \Ubiquity\controllers\SimpleViewAsyncController {

	public function initialize() {
		\Ubiquity\utils\http\UResponse::setContentType('text/html', 'utf-8');
	}

	public function index() {
		$fortunes = \Ubiquity\orm\DAO::executePrepared('fortune');
		$fortunes[] = (new Fortune())->setId(0)->setMessage('Additional fortune added at request time.');
		\usort($fortunes, function ($left, $right) {
			return $left->message <=> $right->message;
		});
		$this->loadView('Fortunes/index.php', [
			'fortunes' => $fortunes
		]);
	}
}

