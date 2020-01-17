<?php
namespace controllers;

use Ubiquity\orm\SDAO;
use models\Fortune;
use Ubiquity\controllers\Startup;

class Fortunes extends \Ubiquity\controllers\SimpleViewController {

	public function initialize() {
		\Ubiquity\cache\CacheManager::startProd(Startup::$config);
	}

	public function index() {
		$fortunes = SDAO::getAll(Fortune::class);
		$fortunes[] = (new Fortune())->setId(0)->setMessage('Additional fortune added at request time.');
		\usort($fortunes, function ($left, $right) {
			return $left->message <=> $right->message;
		});
		$this->loadView('Fortunes/index.php', [
			'fortunes' => $fortunes
		]);
	}
}

