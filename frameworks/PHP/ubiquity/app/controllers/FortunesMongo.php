<?php
namespace controllers;

use Ubiquity\orm\core\prepared\DAONosqlPreparedQueryAll;

class FortunesMongo extends Fortunes_ {

	public static function warmup() {
		self::$pDao = new DAONosqlPreparedQueryAll('models\\Fortune');
	}
}

