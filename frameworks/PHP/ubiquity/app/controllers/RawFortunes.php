<?php
namespace controllers;

use Ubiquity\controllers\Controller;
use Ubiquity\db\Database;

class RawFortunes extends Controller {
	
	public function index() {
		$fortunes = Database::start()->fetchAll('SELECT id,message FROM Fortune');
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


