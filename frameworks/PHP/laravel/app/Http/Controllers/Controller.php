<?php
namespace App\Http\Controllers;

use App\Models\Fortune;
use App\Models\World;
use Illuminate\Routing\Controller as BaseController;

class Controller extends BaseController {

	public function json() {
		return [
			'message' => 'Hello, World!'
		];
	}

	public function db() {
		return World::find(\mt_rand(1, 10000));
	}

	public function queries($queries = 1) {
		$rows = [];
		$numbers = $this->getUniqueRandomNumbers($this->clamp($queries));
		foreach ($numbers as $id) {
			$rows[] = World::find($id);
		}

		return $rows;
	}

	public function fortunes() {
		$rows = Fortune::all();

		$insert = new Fortune();
		$insert->id = 0;
		$insert->message = 'Additional fortune added at request time.';

		$rows->add($insert);
		$rows = $rows->sortBy('message');

		return view('fortunes', [
			'rows' => $rows
		]);
	}

	public function updates($queries = 1) {
		$rows = [];

		$numbers = $this->getUniqueRandomNumbers($this->clamp($queries));
		foreach ($numbers as $id) {
			$row = World::find($id);
			$oldId = $row->randomNumber;
			do {
				$newId = mt_rand(1, 10000);
			} while ($oldId === $newId);
			$row->randomNumber = $newId;
			do {
				$saved = $row->save();
			} while (! $saved);
			$rows[] = $row;
		}

		return $rows;
	}

	public function plaintext() {
		return response('Hello, World!')->header('Content-Type', 'text/plain');
	}

	private function clamp($value): int {
		if (! \is_numeric($value) || $value < 1) {
			return 1;
		} else if ($value > 500) {
			return 500;
		} else {
			return $value;
		}
	}

	private function getUniqueRandomNumbers($count) {
		$res = [];
		do {
			$res[\mt_rand(1, 10000)] = 1;
		} while (\count($res) < $count);
		\ksort($res);
		return \array_keys($res);
	}
}
