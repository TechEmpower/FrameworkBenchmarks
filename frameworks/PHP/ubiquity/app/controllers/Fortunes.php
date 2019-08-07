<?php
namespace controllers;

use Ubiquity\controllers\Controller;
use Ubiquity\controllers\Startup;
use Ubiquity\orm\DAO;
use Ubiquity\views\engine\micro\MicroTemplateEngine;
use models\Fortune;

class Fortunes extends Controller {

	public function initialize() {
		Startup::$templateEngine = new MicroTemplateEngine();
		DAO::setModelDatabase(Fortune::class);
	}

	public function index() {
		$fortunes = DAO::getAll(Fortune::class, '', false);
		$fortunes[] = (new Fortune())->setId(0)->setMessage('Additional fortune added at request time.');
		usort($fortunes, function ($left, $right) {
			return $left->message <=> $right->message;
		});
		$this->loadView('Fortunes/index.php', [
			'fortunes' => $fortunes
		]);
	}
}

