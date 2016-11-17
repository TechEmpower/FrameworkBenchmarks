<?php

namespace app\controllers;

use  lithium\action\Controller;
use  app\models\World;
use  app\models\Fortune;

class BenchController extends Controller {

    public function plaintext() {
      return $this->render(array('text' => 'Hello, World!'));
    }

    public function json() {
        return $this->render(array(
            'json' => array('message' => 'Hello, World!')
        ));
    }

    public function db() {
        $queries = isset($this->request->query['queries'])
            ? $this->request->query['queries']
            : 1;
        $queries = min(max(1, $queries), 500);
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

    public function update() {
      $queries = isset($this->request->query['queries'])
          ? $this->request->query['queries']
          : 1;
      $queries = min(max(1, $queries), 500);
      $worlds = array();

      for ($i = 0; $i < $queries; ++$i) {
          $id = mt_rand(1, 10000);
          $random_number = mt_rand(1, 10000);
          $world = World::first(array(
              'conditions' => array(
                  'id' => $id
              )
          ));
          $world->randomNumber = $random_number;
          $world->save();
          $worlds[] = $world;
      }

      return $this->render(array(
          'json' => $worlds
      ));
    }

    public function fortunes() {
        $fortunes = Fortune::find('all')->to('array');
        $fortunes[] = array('id' => 0, 'message' => 'Additional fortune added at request time.');
        usort($fortunes, function($left, $right) {
           return strcmp($left['message'], $right['message']);
        });

        $this->set(['fortunes' => $fortunes]);
        return $this->render(array('layout' => false));
    }
}
