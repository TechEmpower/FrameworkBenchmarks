<?php

namespace App\Controller;

use Cake\Controller\Controller;

class FortunesController extends Controller {

    protected $modelClass = 'Fortune';

    public function index() 
    {
        $query = $this->Fortune->find('all');

        // Calling all() will execute the query
        // and return the result set.
        $fortunes = $query->all();

        // stuffing in the dynamic data
        $fortune = $this->Fortune->newEntity([
            'id' => 0,
            'message' => 'Additional fortune added at request time.'
        ]);

        $fortunes = $fortunes->appendItem($fortune);

        $fortunes = $fortunes->sortBy('message', SORT_ASC, SORT_STRING)->compile(false);

        $this->set('fortunes', $fortunes);
    }

}
