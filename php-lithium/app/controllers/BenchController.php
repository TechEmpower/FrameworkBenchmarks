<?php

namespace app\controllers;

use  lithium\action\Controller;
use  app\models\World;

class BenchController extends Controller {

    public function json() {
        return $this->render(array(
            'json' => array('message' => 'Hello World!')
        ));
    }

    public function db() {
        $queries = isset($this->request->query['queries'])
            ? $this->request->query['queries']
            : 1;
        $worlds = array();

        for ($i = 0; $i < $queries; ++$i) {
            $worlds[] = World::first(array(
                'conditions' => array(
                    'id' => mt_rand(1, 10000)
                )
            ));
        }

        return $this->render(array(
            'json' => $worlds
        ));
    }
}