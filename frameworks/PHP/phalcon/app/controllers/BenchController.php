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

    public function jsonAction()
    {
        return $this->response->setJsonContent(array(
            'message' => 'Hello, World!'
        ));
    }

    public function dbAction()
    {
        return $this->response->setJsonContent($this->getRandomWorld());
    }

    public function queriesAction()
    {

	$queries = min(500, max(1, $this->filter->sanitize($this->request->getQuery('queries', null, 1), "int")));

        $worlds = array();

        for ($i = 0; $i < $queries; ++$i) {
            $worlds[] = $this->getRandomWorld();
        }

        return $this->response->setJsonContent($worlds);
    }

    public function fortunesAction()
    {

        $fortunes = $this->getFortunesArray();
        $fortunes[] = $this->buildFortune();

        $this->response->setHeader("Content-Type", "text/html; charset=utf-8");

        $this->view->fortunes = $this->sortFortunes($fortunes);
    }

    public function updateAction()
    {

        $queries = $this->request->getQuery('queries', null, 1);
        $queries = max(1, min(500, $queries));

        $worlds = array();

        for ($i = 0; $i < $queries; ++$i) {
            $world = $this->getRandomWorld();
            $world->randomNumber = mt_rand(1, 10000);
            $world->save();
            $worlds[] = $world;
        }

        return $this->response->setJsonContent($worlds);
    }

    public function plaintextAction()
    {
        $this->view->disable();
        $this->response->setContentType('text/plain');
        $this->response->setContent("Hello, World!");
        return $this->response;
    }

    protected function getRandomWorld()
    {
        return Worlds::findFirst(mt_rand(1, 10000));
    }

    protected function getFortunesArray()
    {
        // since the resultset is immutable get an array instead
        // so we can add the new fortune
        return Fortunes::find()->toArray();
    }

    protected function buildFortune()
    {
        return array(
            'id' => 0,
            'message' => 'Additional fortune added at request time.'
        );
    }

    protected function sortFortunes($fortunes)
    {
        usort($fortunes,
                function($left, $right) {
                    return $left['message'] <=> $right['message'];
                });
        return $fortunes;
    }
}
