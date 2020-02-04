<?php
//
// JSON Encoding Test
//
namespace App\Controller;

use Cake\Controller\Controller;

class JsonController extends Controller {

    public function index() 
    {
        $data = ['message' => 'Hello, World!'];

        return $this->response
            ->withStringBody(\json_encode($data))
            ->withType('application/json');
    }
}
