<?php
//
// Database Mapping Test
//
namespace App\Controller;

use Cake\ORM\TableRegistry;

class WorldController extends AppController {


    public function initialize()
    {
        parent::initialize();
        $this->loadComponent('RequestHandler');
    }

    protected function _getQueryCount()
    {
        $query_count = $this->request->getQuery('queries');

        $query_count = (int)$query_count;
        if ($query_count === 0) 
        {
            $query_count = 1;
        } elseif ($query_count > 500) {
            $query_count = 500;
        }

        return $query_count;
    }

    public function index() 
    {
        $this->viewBuilder()->setLayout('plaintext');
        // Create an array with the response string.
        $worlds = array();
        $worldTable = TableRegistry::getTableLocator()->get('World');
        $query_count = $this->_getQueryCount();

        // For each query, store the result set values in the response array
        for ($i = 0; $i < $query_count; $i++) {
            $worlds[] = $worldTable->get(rand(1,10000));
        }
	$this->set('worlds', $worlds);

        $this->set('_serialize', 'worlds');
        $this->RequestHandler->renderAs($this, 'json');
    }

    public function query()
    {
        $this->viewBuilder()->setLayout('plaintext');
        $worldTable = TableRegistry::getTableLocator()->get('World');
        $world = $worldTable->get(rand(1,10000));

        return $this->response->withType("application/json")->withStringBody(json_encode($world, JSON_FORCE_OBJECT));
    }

    public function updates()
    {
        $this->viewBuilder()->setLayout('plaintext');
        $worldTable = TableRegistry::getTableLocator()->get('World');

        // Create an array with the response string.
        $worlds = array();

        $query_count = $this->_getQueryCount();

        // For each query, store the result set values in the response array
        for ($i = 0; $i < $query_count; $i++)
        {
            $world = $worldTable->get(rand(1,10000));

            $newWorld = $worldTable->get(rand(1,10000));
            $newWorld->randomNumber = mt_rand(1, 10000);

            $worldTable->save($newWorld);
            $worlds[] = $world;
	}

        // Return json list
        $this->set('worlds', $worlds);

        $this->set('_serialize', 'worlds');
        $this->RequestHandler->renderAs($this, 'json');
    }

}
