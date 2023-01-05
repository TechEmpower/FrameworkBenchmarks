<?php

namespace app\controllers;

use mako\http\routing\Controller;
use mako\view\ViewFactory;
use mako\http\response\builders\JSON;
use app\models\World;
use app\models\Fortune;

/**
 * Welcome controller.
 */
class Index extends Controller
{
	/**
	 * plaintext route.
	 *
	 * @return string
	 */
	public function plaintext(): string
	{
		$this->response->setType('text/plain');
		return 'Hello, World!';
	}

	/**
	 * json route.
	 *
	 * @return string
	 */
	public function json(): JSON
	{
		return $this->jsonResponse(['message' => 'Hello, World!']);
	}

	/**
	 * db route.
	 *
	 * @return string
	 */
	public function db(): JSON
	{
		$id = mt_rand(1, 10000);
		$World = new World;
		$world = $World->get($id);
		// $world = [
		// 	'id' => $World->id,
		// 	'randomNumber' => $World->randomNumber
		// ];
		return $this->jsonResponse($world);
	}

	/**
	 * queries route.
	 *
	 * @return string
	 */
	public function queries(): JSON
	{
		$queries = $this->request->getQuery()->get('queries');
		if (is_numeric($queries)) {
			$queries = max(1, min($queries, 500));
		} else {
			$queries = 1;
		}
		$worlds = [];
		$World = new World;
		for ($i = 0; $i < $queries; ++$i) {
			$id = mt_rand(1, 10000);
			$world = $World->get($id);
			$worlds[] = $world;
		}

		return $this->jsonResponse($worlds);
	}

	/**
	 * fortunes route.
	 *
	 * @param  \mako\view\ViewFactory $view View factory
	 * @return string
	 */
	public function fortunes(ViewFactory $view): string
	{
		$Fortune = new Fortune;
		$fortunes = $Fortune->all()->getItems();
		$fortunes[] = (object) [ 'id' => 0, 'message' => 'Additional fortune added at request time.' ];
		usort($fortunes, function ($left, $right) {
            return $left->message <=> $right->message;
        });
		return $view->render('fortunes', [ 'fortunes' => $fortunes ]);
	}

	/**
	 * update route.
	 */
	public function update(): JSON
	{
		$queries = $this->request->getQuery()->get('queries');
		if (is_numeric($queries)) {
			$queries = max(1, min($queries, 500));
		} else {
			$queries = 1;
		}
		$worlds = [];
		$World = new World;
		for ($i = 0; $i < $queries; ++$i) {
			$id = mt_rand(1, 10000);
			$random_number = mt_rand(1, 10000);
			$world = $World->get($id);
			$world->randomNumber = $random_number;
			$world->save();
			$worlds[] = $world;
		}

		return $this->jsonResponse($worlds);
	}
}
