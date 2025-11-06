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
		$row = Container::get('db')->select('World', 'id = ?', mt_rand(1, 10000))[0];

		$res->setHeader('Content-Type', 'application/json');
		$res->writeJson($row);
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
			$worlds[] = $db->select('World', 'id = ?', mt_rand(1, 10000))[0];
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
			$worlds[] = [
						'id' => $row['id'],
						'randomNumber' => $random_update_id
						];
		}
		$res->setHeader('Content-Type', 'application/json');
		$res->writeJson($worlds);
	}

	public function fortunes(Request $req, Response $res)
	{

		$fortunes = Container::get('db')->select('Fortune');
		$fortunes[] = [ 'id' => 0, 'message' => 'Additional fortune added at request time.' ];
		usort($fortunes, fn($left, $right) => $left['message'] <=> $right['message'] );
		
		$res->setHeader('Content-Type', 'text/html; charset=utf-8');
		View::render('fortunes', [
			'fortunes' => $fortunes,
		]);
	}
}
