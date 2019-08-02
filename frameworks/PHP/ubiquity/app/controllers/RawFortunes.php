<?php
namespace controllers;

class RawFortunes extends \Ubiquity\controllers\Controller {
	
	public function initialize() {
		\Ubiquity\controllers\Startup::$templateEngine = new \Ubiquity\views\engine\micro\MicroTemplateEngine();
	}
	
	public function index() {
		$fortunes = \Ubiquity\db\Database::start()->fetchAll('SELECT id,message FROM Fortune',\PDO::FETCH_KEY_PAIR);
		$fortunes[0] = 'Additional fortune added at request time.';
		\asort($fortunes);
		$this->loadView('Fortunes/index.php', [
			'fortunes' => $fortunes
		]);
	}
}


