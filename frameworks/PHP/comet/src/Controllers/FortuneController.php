<?php
declare(strict_types=1);

namespace App\Controllers;

use App\ORM;
use App\Storage;
use Comet\Request;
use Comet\Response;

class FortuneController
{
    public function __invoke(Request $request, Response $response, $args)    
    {
   		ORM::$fortune->execute();

        $arr = ORM::$fortune->fetchAll(\PDO::FETCH_KEY_PAIR);
        $arr[0] = 'Additional fortune added at request time.';
        asort($arr);

        $html = '';
        foreach ($arr as $id => $message) {
        	$message = htmlspecialchars($message, ENT_QUOTES, 'UTF-8');
	        $html .= "<tr><td>$id</td><td>$message</td></tr>";
    	}

    	return $response
    		->with(
    			'<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>'
        		.$html.
        		'</table></body></html>'
        	)
        	->withHeader('Content-Type', 'text/html; charset=utf-8')
        	->withHeader('Date', Storage::$date);
	}
}

