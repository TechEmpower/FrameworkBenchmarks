<?php

class BenchController extends \Phalcon\Mvc\Controller
{
    public function initialize()
    {
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

    private function getQueryOrDefault($query, $default) {
        return $this->request->getQuery($query) !== null
            ? $this->request->getQuery($query)
            : $default;
    }

    private function sendContentAsJson($content) {
        $response = new Phalcon\Http\Response();
        $response->setStatusCode(200, "OK");
        $response->setHeader("Content-Type", "application/json");
        $response->setContent(json_encode($content));
        return $response;
    }
}