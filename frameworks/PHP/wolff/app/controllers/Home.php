<?php

namespace Controller;

use Wolff\Core\Container;
use Wolff\Core\Http\Request;
use Wolff\Core\Http\Response;
use Wolff\Core\Language;
use Wolff\Core\View;

class Home extends \Wolff\Core\Controller
{

	public function plaintext(Request $req, Response $res)
	{
		$res->setHeader('Content-Type', 'text/plain');
		$res->write('Hello, World!');
	}

	public function json(Request $req, Response $res)
	{
		$res->setHeader('Content-Type', 'application/json');
		$res->writeJson(['message' => 'Hello, World!']);
	}

	public function db(Request $req, Response $res)
	{
		$random_id = mt_rand(1, 10000);
		/** @var \Wolff\Core\DB */
		$db = Container::get('db');
		$row = $db->select('World', 'id = ?', $random_id)[0];
		$res->setHeader('Content-Type', 'application/json');
		$res->writeJson([
			'id' => $row['id'],
			'randomNumber' => $row['randomNumber']
		]);
	}

	public function queries(Request $req, Response $res)
	{
		$queries = $req->query('queries');
		if (is_numeric($queries)) {
			$queries = max(1, min($queries, 500));
		} else {
			$queries = 1;
		}

		/** @var \Wolff\Core\DB */
		$db = Container::get('db');
		
		$worlds = [];
		for ($i = 0; $i < $queries; ++$i) {
			$random_id = mt_rand(1, 10000);
			$row = $db->select('World', 'id = ?', $random_id)[0];
			$world = [
				'id' => $row['id'],
				'randomNumber' => $row['randomNumber']
			];
			$worlds[] = $world;
		}
		$res->setHeader('Content-Type', 'application/json');
		$res->writeJson($worlds);
	}

	public function update(Request $req, Response $res)
	{
		$queries = $req->query('queries');
		if (is_numeric($queries)) {
			$queries = max(1, min($queries, 500));
		} else {
			$queries = 1;
		}

		/** @var \Wolff\Core\DB */
		$db = Container::get('db');
		
		$worlds = [];
		for ($i = 0; $i < $queries; ++$i) {
			$random_id = mt_rand(1, 10000);
			$random_update_id = mt_rand(1, 10000);
			$row = $db->select('World', 'id = ?', $random_id)[0];
			$db->query('UPDATE World SET randomNumber = ? WHERE id = ?', $random_update_id, $row['id']);
			$world = [
				'id' => $row['id'],
				'randomNumber' => $random_update_id
			];
			$worlds[] = $world;
		}
		$res->setHeader('Content-Type', 'application/json');
		$res->writeJson($worlds);
	}

	public function fortunes(Request $req, Response $res)
	{

		/** @var \Wolff\Core\DB */
		$db = Container::get('db');

		$fortunes = $db->select('Fortune');
		$fortunes[] = [ 'id' => 0, 'message' => 'Additional fortune added at request time.' ];
		usort($fortunes, function ($left, $right) {
            return $left['message'] <=> $right['message'];
        });
		View::render('fortunes', [
			'fortunes' => $fortunes,
		]);
	}
}
