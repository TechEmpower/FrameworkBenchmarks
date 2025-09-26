<?php
declare(strict_types=1);

namespace App\Controllers;

use App\ORM;
use App\Storage;
use Comet\Request;
use Comet\Response;

class UpdateController
{
    public function __invoke(Request $request, Response $response, $args)    
    {
    	$queryParams = $request->getQueryParams();
    	$q = (int) $queryParams['q'] ?? 0;
        $query_count = $q > 1 ? min($q, 500) : 1;

		// TODO Speedup with signle transaction?
		// ORM::$pdo->beginTransaction();
    	while ($query_count--) {
        	$id = mt_rand(1, 10000);
        	ORM::$random->execute([$id]);
        	$world = ['id' => $id, 'randomNumber' => ORM::$random->fetchColumn()];        	
        	ORM::$update->execute(
            	[ $world['randomNumber'] = mt_rand(1, 10000), $id ]
        	);
        	$arr[] = $world;
    	}

    	return $response
    		->with($arr)
	    	->withHeader('Date', Storage::$date);		
	}
}

