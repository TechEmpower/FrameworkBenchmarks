<?php
namespace controllers;

use controllers\utils\DbTrait;

/**
 * Bench controller.
 */
class DbRaw extends \Ubiquity\controllers\Controller {
	use DbTrait;

	protected static $statement;

	protected static $uStatements;

	/**
	 *
	 * @var \Ubiquity\db\Database
	 */
	protected static $db;

	private static function prepareUpdate(int $count) {
		$sql = 'UPDATE World SET randomNumber = CASE id' . \str_repeat(' WHEN ?::INTEGER THEN ?::INTEGER ', $count) . 'END WHERE id IN (' . \str_repeat('?::INTEGER,', $count - 1) . '?::INTEGER)';
		return self::$uStatements[$count] = self::$db->prepareStatement($sql);
	}

	public function __construct() {}

	public static function warmup(\Ubiquity\db\Database $db) {
		self::$db = $db;
		self::$statement = $db->prepareStatement('SELECT id,randomNumber FROM World WHERE id=?');
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
		$count = $this->getCount($queries);
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
		$keys = $values = [];
		$count = $this->getCount($queries);
		for ($i = 0; $i < $count; ++ $i) {
			$values[] = $keys[] = $id = \mt_rand(1, 10000);
			self::$statement->execute([
				$id
			]);
			$row = self::$statement->fetch();

			$values[] = $row['randomNumber'] = \mt_rand(1, 10000);
			$worlds[] = $row;
		}
		(self::$uStatements[$count] ?? self::prepareUpdate($count))->execute([
			...$values,
			...$keys
		]);
		echo \json_encode($worlds);
	}
}
