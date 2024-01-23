<?php

declare(strict_types=1);

namespace App\Presenters;

use Nette;


final class DatabasePresenter extends Nette\Application\UI\Presenter
{

	private Nette\Database\Explorer $explorer;

	public function __construct(Nette\Database\Explorer $explorer) {
		$this->explorer = $explorer;
	}

	public function renderDb() {
		$random_id = mt_rand(1, 10000);
		$table = $this->explorer->table('World');
		$row = $table->where('id', $random_id)->fetch();
		$this->sendJson([
			'id' => $row->id,
			'randomNumber' => $row->randomNumber
		]);
	}

	public function renderQuery() {
		$queries = $this->getHttpRequest()->getQuery('queries');
		if (is_numeric($queries)) {
			$queries = max(1, min($queries, 500));
		} else {
			$queries = 1;
		}
		
		$worlds = [];
		for ($i = 0; $i < $queries; ++$i) {
			$random_id = mt_rand(1, 10000);
			$table = $this->explorer->table('World');
			$row = $table->where('id = ?', $random_id)->fetch();
			$world = [
				'id' => $row['id'],
				'randomNumber' => $row['randomNumber']
			];
			$worlds[] = $world;
		}
		$this->sendJson($worlds);
	}

	public function renderUpdate() {
		$queries = $this->getHttpRequest()->getQuery('queries');
		if (is_numeric($queries)) {
			$queries = max(1, min($queries, 500));
		} else {
			$queries = 1;
		}
		
		$worlds = [];
		for ($i = 0; $i < $queries; ++$i) {
			$random_id = mt_rand(1, 10000);
			$random_update_id = mt_rand(1, 10000);
			$update_result = $this->explorer->table('World')->where('id', $random_id);
			$update_row = $update_result->fetch();
			$update_result->update([
				'randomNumber' => $random_update_id
			]);
			$world = [
				'id' => $update_row->id,
				'randomNumber' => $random_update_id
			];
			$worlds[] = $world;
		}
		$this->sendJson($worlds);
	}

	public function renderFortune() {
		$fortunes = $this->explorer->table('Fortune')->fetchAll();
		$fortunes = $this->addFortune($fortunes);
   		$fortunes = $this->sortFortunes($fortunes);
		$this->template->fortunes = $fortunes;
	}

	protected function addFortune($fortunes) {
		$fortunes[] = (object) [ 'id' => 0, 'message' => 'Additional fortune added at request time.' ];
		return $fortunes;
	}

	protected function sortFortunes(array $fortunes): array {
        usort($fortunes, function ($left, $right) {
            return $left->message <=> $right->message;
        });

        return $fortunes;
    }
}
