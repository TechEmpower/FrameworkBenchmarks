<?php

//App::uses('AppController', 'Controller');

namespace App\Controller;

use App\Model\Entity\Fortune;
use Cake\ORM\TableRegistry;

class FortunesController extends AppController {

    public function index() {
        $this->viewBuilder()->setLayout('fortunes');
        $this->loadModel('Fortune');
	$this->set('title_for_layout', 'Fortunes');

        $fortunesTable = TableRegistry::getTableLocator()->get('Fortune');

        $query = $fortunesTable->find('all');

        // Calling all() will execute the query
        // and return the result set.
        $results = $query->all()->toArray();

        // stuffing in the dynamic data
        $fortune = array(
            'id' => 0,
            'message' => 'Additional fortune added at request time.'
        );

        $results[] = $fortune;

        // because we are performance concerned we don't use Hash::sort()
        foreach ($results as $result) {
            $fortunes[$result['id']] = $result['message'];
        }
        asort($fortunes);

        $this->set('fortunes', $fortunes);
    }

}
