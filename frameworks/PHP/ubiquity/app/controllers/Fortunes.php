<?php
namespace controllers;

use Ubiquity\orm\SDAO;
use models\Fortune;

class Fortunes extends \Ubiquity\controllers\SimpleViewController {

	public function initialize() {
		\Ubiquity\cache\CacheManager::startProdFromCtrl();
	}

	public function index() {
		$fortunes = SDAO::getAll(Fortune::class);
		$fortunes[] = new Fortune(0, 'Additional fortune added at request time.');
		\usort($fortunes, function ($left, $right) {
			return $left->message <=> $right->message;
		});
		$this->loadView('Fortunes/index.php', [
			'fortunes' => $fortunes
		]);
	}
}

