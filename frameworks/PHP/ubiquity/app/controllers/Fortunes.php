<?php
namespace controllers;

use Ubiquity\orm\DAO;
use models\Fortune;
use Ubiquity\controllers\Startup;

class Fortunes extends \Ubiquity\controllers\Controller {

	public function initialize() {
		\Ubiquity\cache\CacheManager::startProd(Startup::$config);
		Startup::$templateEngine = new \Ubiquity\views\engine\micro\MicroTemplateEngine();
		DAO::setModelDatabase(Fortune::class);
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

