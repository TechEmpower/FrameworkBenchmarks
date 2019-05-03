<?php

namespace App\Controller;

class PlaintextController extends AppController {

    public function initialize()
    {
        parent::initialize();
        $this->loadComponent('RequestHandler');
    }

    public function index() 
    {
        $this->viewBuilder()->setLayout('plaintext');

        $this->response = $this->response->withStringBody("Hello, World!");
        $this->response = $this->response->withType('text/plain');

        return $this->response;
    }

}
