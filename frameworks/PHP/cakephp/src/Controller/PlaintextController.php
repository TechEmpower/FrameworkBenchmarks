<?php

namespace App\Controller;

use Cake\Controller\Controller;

class PlaintextController extends Controller {

    public function index() 
    {
        return $this->response
            ->withStringBody('Hello, World!')
            ->withType('text/plain');
    }

}
