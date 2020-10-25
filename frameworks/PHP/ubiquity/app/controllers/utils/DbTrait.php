<?php
namespace controllers\utils;

trait DbTrait {

	public function getCount($queries) {
		$count = 1;
		if ($queries > 1) {
			if (($count = $queries) > 500) {
				$count = 500;
			}
		}
		return $count;
	}
}
