<?php

class World extends AppModel
{
	public $useTable = 'World'; // This model uses a database table 'World'
	public $primaryKey = 'id';
	public $findMethods = ['randomId' => true];

	protected function _findRandomId($state, $query, $results = array())
	{
		if ($state === 'before') {
			// Choose a random row
			// http://www.php.net/mt_rand
			$id = mt_rand(1, 10000);
			$query['conditions']['id'] = $id;
			$query['limit'] = 1;

			return $query;
		}

		return $results[0]['World'];
	}
}
