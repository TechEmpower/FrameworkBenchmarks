<?php

use Phalcon\Mvc\View,
    Phalcon\Mvc\Model\Resultset;

class BenchController extends \Phalcon\Mvc\Controller
{
    public function initialize()
    {
        // views must be renderd explicitly. safes processing time when not needed.
        $this->view->setRenderLevel(View::LEVEL_LAYOUT);
    }

    public function jsonAction() {
        return $this->sendContentAsJson(array(
            'message' => 'Hello World!'
        ));
    }

    public function dbAction() {

        $queries = $this->request->getQuery('queries', null, 1);

        $worlds = array();

        for ($i = 0; $i < $queries; ++$i) {
            $worlds[] = Worlds::findFirst(mt_rand(1, 10000));
        }

        return $this->sendContentAsJson($worlds);
    }

    public function fortunesAction() {

        // since the resultset is immutable get an array instead
        // so we can add the new fortune
        $fortunes = Fortunes::find()->toArray();

        $fortunes[] = array(
            'id' => 0,
            'message' => 'Additional fortune added at request time.'
        );

        usort($fortunes, function($left, $right) {
            $l = $left['message'];
            $r = $right['message'];
            if ($l === $r) {
                return 0;
            } else {
                if ($l > $r) {
                    return 1;
                } else {
                    return -1;
                }
            }
        });

        $this->response->setStatusCode(200, "OK");
        $this->response->setHeader("Content-Type", "text/html; charset=utf-8");

        $this->view->fortunes = $fortunes;
    }

    private function sendContentAsJson($content) {
        $response = new Phalcon\Http\Response(json_encode($content));
        $response->setStatusCode(200, "OK");
        $response->setHeader("Content-Type", "application/json");
        return $response;
    }
}