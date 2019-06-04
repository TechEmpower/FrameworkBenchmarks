<?php

namespace App\Controller;

use App\Model\Entity\Fortune;
use Cake\ORM\TableRegistry;

class FortunesController extends AppController {

    public function index() {
        $this->viewBuilder()->setLayout('fortunes');
        $this->loadModel('Fortune');

        $fortunesTable = TableRegistry::getTableLocator()->get('Fortune');

        $query = $fortunesTable->find('all');

        // Calling all() will execute the query
        // and return the result set.
        $fortunes = $query->all();



	    // stuffing in the dynamic data
	    $fortune = TableRegistry::getTableLocator()->get('Fortune')->newEntity([
	    	'id' => 0,
		    'message' => 'Additional fortune added at request time.'
	    ]);

	    $fortunes = $fortunes->appendItem($fortune);

	    $fortunes = $fortunes->sortBy('message', SORT_ASC, SORT_STRING)->compile(false);

        $this->set('fortunes', $fortunes);
    }

}
