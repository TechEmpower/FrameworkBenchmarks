<?php

namespace App\Controller;

use Cake\Controller\Controller;

class MainController extends Controller
{
    public function json()
    {
        $data = ['message' => 'Hello, World!'];

        return $this->_jsonResponse($data);
    }

    public function db()
    {
        $world = $this->fetchTable('World')->get(self::_randomInt());

        return $this->_jsonResponse($world);
    }

    public function fortunes()
    {
        // Calling all() will execute the query
        // and return the result set.
        $fortunes = $this->fetchTable('Fortune')->find()->all();

        // stuffing in the dynamic data
        $fortune = $this->fetchTable('Fortune')->newEntity([
            'id' => 0,
            'message' => 'Additional fortune added at request time.',
        ]);

        $fortunes = $fortunes->appendItem($fortune);

        $fortunes = $fortunes->sortBy('message', SORT_ASC, SORT_STRING)->compile(false);

        $this->set('fortunes', $fortunes);
    }

    public function queries()
    {
        // Create an array with the response string.
        $worlds = [];
        $queries = $this->_getQueryCount();
        $worldRepo = $this->fetchTable('World');

        // For each query, store the result set values in the response array
        while ($queries--) {
            $worlds[] = $worldRepo->get(self::_randomInt());
        }

        return $this->_jsonResponse($worlds);
    }

    public function updates()
    {
        // Create an array with the response string.
        $worlds = [];
        $queries = $this->_getQueryCount();
        $worldRepo = $this->fetchTable('World');

        // For each query, store the result set values in the response array
        while ($queries--) {
            $world = $worldRepo->get(self::_randomInt());
            $world->randomNumber = self::_randomInt();

            $worldRepo->save($world);
            $worlds[] = $world;
        }

        return $this->_jsonResponse($worlds);
    }

    public function plaintext()
    {
        return $this->response
            ->withStringBody('Hello, World!')
            ->withType('text/plain');
    }

    protected function _jsonResponse($data)
    {
        return $this->response
            ->withType('application/json')
            ->withStringBody(\json_encode($data));
    }

    protected function _getQueryCount()
    {
        $queries = (int)$this->request->getQuery('queries');

        if ($queries === 0) {
            $queries = 1;
        } elseif ($queries > 500) {
            $queries = 500;
        }

        return $queries;
    }

    protected static function _randomInt()
    {
        return random_int(1, 10000);
    }
}
