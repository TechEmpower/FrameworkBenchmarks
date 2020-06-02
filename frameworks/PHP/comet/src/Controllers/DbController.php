<?php
declare(strict_types=1);

namespace App\Controllers;

use App\ORM;
use Comet\Request;
use Comet\Response;

class DbController
{
    public function __invoke(Request $request, Response $response, $args)    
    {
    	ORM::$statement->execute([mt_rand(1, 10000)]);

    	return $response
    		->with(ORM::$statement->fetch())
	    	->withHeader('Date', gmdate('D, d M Y H:i:s') . ' GMT');
	}
}

