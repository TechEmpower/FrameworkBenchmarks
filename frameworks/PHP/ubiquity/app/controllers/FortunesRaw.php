<?php
namespace controllers;

class FortunesRaw extends \Ubiquity\controllers\SimpleViewAsyncController {

	protected static $statement;

	public static function warmup(\Ubiquity\db\Database $db) {
		self::$statement = $db->prepareStatement('SELECT id,message FROM Fortune');
	}

	public function index() {
		self::$statement->execute();
		$fortunes = self::$statement->fetchAll(\PDO::FETCH_KEY_PAIR);
		$fortunes[0] = 'Additional fortune added at request time.';
		\asort($fortunes);
		$this->loadView('Fortunes/raw.php', ['fortunes' => $fortunes]);
	}
}

