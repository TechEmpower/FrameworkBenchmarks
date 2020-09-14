<?php
namespace controllers\utils;

use Ubiquity\orm\core\prepared\DAOPreparedQueryById;

trait DbAsyncTrait {

	/**
	 *
	 * @var DAOPreparedQueryById
	 */
	protected static $pDao;

	public function __construct() {}

	public function initialize() {
		\Ubiquity\utils\http\UResponse::setContentType('application/json');
	}

	public static function warmup() {
		self::$pDao = new DAOPreparedQueryById('models\\World');
	}
}

