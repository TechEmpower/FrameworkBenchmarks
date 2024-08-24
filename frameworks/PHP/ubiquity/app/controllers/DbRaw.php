<?php
namespace controllers;

use controllers\utils\DbTrait;

/**
 * Bench controller.
 */
class DbRaw extends \Ubiquity\controllers\Controller {

	protected static $statement;

	protected static $uStatements;

	/**
	 *
	 * @var \Ubiquity\db\Database
	 */
	protected static $db;

	public function __construct() {}

	public static function warmup(\Ubiquity\db\Database $db) {
		self::$db = $db;
		self::$statement = $db->prepareStatement('SELECT id,randomNumber FROM World WHERE id=?');
        self::$uStatements = $db->prepareStatement('UPDATE World SET randomNumber=? WHERE id=?');
	}

	public function initialize() {
		\Ubiquity\utils\http\UResponse::setContentType('application/json');
	}

	public function index() {
		self::$statement->execute([
			\mt_rand(1, 10000)
		]);
		echo \json_encode(self::$statement->fetch());
	}

	public function query($queries = 1) {
		$worlds = [];
		$count = \min(\max((int) $queries, 1), 500);
		while ($count --) {
			self::$statement->execute([
				\mt_rand(1, 10000)
			]);
			$worlds[] = self::$statement->fetch();
		}
		echo \json_encode($worlds);
	}

	public function update($queries = 1) {
        $worlds = [];
        $count = \min(\max((int) $queries, 1), 500);
        for ($i = 0; $i < $count; ++ $i) {
            $id = \mt_rand(1, 10000);
            self::$statement->execute([$id]);
            $row = self::$statement->fetch();

            $row['randomNumber'] = \mt_rand(1, 10000);
            self::$uStatements->execute([$row['randomNumber'], $row['id']]);
            $worlds[] = $row;
        }
        echo \json_encode($worlds);
	}
}
