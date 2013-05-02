<?php

class BenchController extends \Phalcon\Mvc\Controller
{
    public function initialize()
    {
        // views must be renderd explicitly. safes processing time when not needed.
        $this->view->disable();
    }

    public function jsonAction() {
        return $this->sendContentAsJson(array(
            'message' => 'Hello World!'
        ));
    }

    public function dbAction() {
        $queries = $this->getQueryOrDefault('queries', 1);
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
            if ($left['message'] === $right['message']) {
                return 0;
            } else if ($left['message'] > $right['message']) {
                return 1;
            } else {
                return -1;
            }
        });

        return $this->sendContentAsText(
            $this->view->getRender('bench', 'fortunes', array(
                'fortunes' => $fortunes
            ))
        );
    }

    private function getQueryOrDefault($query, $default) {
        return $this->request->getQuery($query) !== null
            ? $this->request->getQuery($query)
            : $default;
    }

    private function sendContentAsText($content) {
        $response = new Phalcon\Http\Response();
        $response->setStatusCode(200, "OK");
        $response->setHeader("Content-Type", "text/html; charset=utf-8");
        $response->setContent($content);
        return $response;
    }

    private function sendContentAsJson($content) {
        $response = new Phalcon\Http\Response();
        $response->setStatusCode(200, "OK");
        $response->setHeader("Content-Type", "application/json");
        $response->setContent(json_encode($content));
        return $response;
    }
}