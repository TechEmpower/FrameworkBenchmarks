<?php
//
// Database Mapping Test
//
namespace App\Controller;

use Cake\Controller\Controller;

class WorldController extends Controller {

    protected $modelClass = 'World';

    protected function _getQueryCount()
    {
        $query_count = (int)$this->request->getQuery('queries');

        if ($query_count === 0) {
            $query_count = 1;
        } elseif ($query_count > 500) {
            $query_count = 500;
        }

        return $query_count;
    }

    public function index() 
    {
        // Create an array with the response string.
        $worlds = [];
        $query_count = $this->_getQueryCount();

        // For each query, store the result set values in the response array
        for ($i = 0; $i < $query_count; $i++) {
            $worlds[] = $this->World->get(rand(1,10000));
        }
	    $this->set('worlds', $worlds);

        $this->viewBuilder()
            ->setClassName('Json')
            ->setOption('serialize', 'worlds');
    }

    public function query()
    {
        $world = $this->World->get(rand(1,10000));

        return $this->response
            ->withType('application/json')
            ->withStringBody(\json_encode($world, JSON_FORCE_OBJECT));
    }

    public function updates()
    {
        // Create an array with the response string.
        $worlds = [];

        $query_count = $this->_getQueryCount();

        // For each query, store the result set values in the response array
        for ($i = 0; $i < $query_count; $i++) {
            $world = $this->World->get(rand(1,10000));
            $world->randomNumber = mt_rand(1, 10000);

            $this->World->save($world);
            $worlds[] = $world;
	    }

        // Return json list
        $this->set('worlds', $worlds);

        $this->viewBuilder()
            ->setClassName('Json')
            ->setOption('serialize', 'worlds');
    }

}
