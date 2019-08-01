<?php
namespace controllers;

class RawFortunes extends \Ubiquity\controllers\Controller {
	
	public function index() {
		$fortunes = \Ubiquity\db\Database::start()->fetchAll('SELECT id,message FROM Fortune',\PDO::FETCH_KEY_PAIR);
		$fortunes[0] = 'Additional fortune added at request time.';
		\asort($fortunes);
		?>
		<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>
		<?php
		foreach ($fortunes as $id => $fortune) :
		?>
		<tr><td><?=$id?></td><td><?=\htmlspecialchars($fortune, ENT_QUOTES, 'UTF-8')?></td></tr>
		<?php
		endforeach
		?></table></body></html><?php
	}
}


