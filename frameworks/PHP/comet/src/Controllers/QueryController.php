<?php
declare(strict_types=1);

namespace App\Controllers;

use App\ORM;
use App\Storage;
use Comet\Request;
use Comet\Response;

class QueryController
{
    public function __invoke(Request $request, Response $response, $args)    
    {
    	$queryParams = $request->getQueryParams();
    	$q = (int) $queryParams['q'] ?? 0;
        $query_count = $q > 1 ? min($q, 500) : 1;

    	while ($query_count--) {
        	ORM::$statement->execute([mt_rand(1, 10000)]);
        	$arr[] = ORM::$statement->fetch();
    	}

    	return $response
    		->with($arr)
	    	->withHeader('Date', Storage::$date);		
	}
}

