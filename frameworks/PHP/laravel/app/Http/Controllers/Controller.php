<?php

namespace App\Http\Controllers;

use App\Models\Fortune;
use App\Models\World;
use Illuminate\Routing\Controller as BaseController;

class Controller extends BaseController {

	public function json() {
		return ['message' => 'Hello, World!'];
	}

	public function db() {
		return World::find(random_int(1, 10000));
	}

	public function queries($queries = 1) {
		$queries = $this->clamp($queries);

		$rows = [];
		while ($queries--) {
			$rows[] = World::find(random_int(1, 10000));
		}

		return $rows;
	}

	public function fortunes() {
		$rows = Fortune::all();

		$insert = new Fortune();
		$insert->id = 0;
		$insert->message = "Additional fortune added at request time.";

		$rows->add($insert);
		$rows = $rows->sortBy("message");

		return view("fortunes", ["rows" => $rows]);
	}

	public function updates($queries = 1) {
		$queries = $this->clamp($queries);

		$rows = [];

		while ($queries--) {
			$row = World::find(random_int(1, 10000));
			$row->randomNumber = random_int(1, 10000);
			$row->save();

			$rows[] = $row;
		}

		return $rows;
	}

	public function plaintext() {
		return response("Hello, World!")->header('Content-Type', 'text/plain');
	}

	private function clamp($value): int {
		if (!is_numeric($value) || $value < 1) {
			return 1;
		} else if ($value > 500) {
			return 500;
		} else {
			return $value;
		}
	}
}
