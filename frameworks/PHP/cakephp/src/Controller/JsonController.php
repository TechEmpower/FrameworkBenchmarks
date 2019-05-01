<?php
//
// JSON Encoding Test
//
namespace App\Controller;

use Cake\Datasource\ConnectionManager;

class JsonController extends AppController {

     public function initialize()
     {
         parent::initialize();
         $this->loadComponent('RequestHandler');
     }

     public function index() {
         $this->set('message', "Hello, World!");
         $this->set('_serialize', array('message'));
         $this->RequestHandler->renderAs($this, 'json');
     }
}
