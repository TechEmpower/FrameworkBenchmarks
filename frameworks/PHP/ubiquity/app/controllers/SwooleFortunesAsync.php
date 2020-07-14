<?php
namespace controllers;

use Ubiquity\orm\DAO;
use models\Fortune;

class SwooleFortunesAsync extends \Ubiquity\controllers\SimpleViewAsyncController {

	public function index() {
		$dbInstance = DAO::pool('async');
		$fortunes = DAO::executePrepared('fortune');
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

