<?php

use Phalcon\Mvc\Controller;
use Phalcon\Mvc\View;
use Phalcon\Mvc\Model\Resultset;
use Phalcon\Http\ResponseInterface;

class BenchController extends Controller
{
    public function initialize(): void
    {
        // views must be rendered explicitly. safes processing time when not needed.
        $this->view->setRenderLevel(View::LEVEL_LAYOUT);
    }

    public function jsonAction(): ResponseInterface
    {
        return $this->response->setJsonContent([
            'message' => 'Hello, World!'
        ]);
    }

    public function dbAction(): ResponseInterface
    {
        return $this->response->setJsonContent($this->getRandomWorld());
    }

    public function queriesAction(): ResponseInterface
    {
        $queries = min(500, max(1, $this->request->getQuery('queries', "int", 1)));
        $worlds = [];

        for ($i = 0; $i < $queries; ++$i) {
            $worlds[] = $this->getRandomWorld();
        }

        return $this->response->setJsonContent($worlds);
    }

    public function fortunesAction(): void
    {
        $fortunes = $this->getFortunesArray();
        $fortunes[] = $this->buildFortune();

        $this->response->setHeader("Content-Type", "text/html; charset=utf-8");

        $this->view->fortunes = $this->sortFortunes($fortunes);
    }

    public function updateAction(): ResponseInterface
    {
        $queries = $this->request->getQuery('queries', "int", 1);
        $queries = max(1, min(500, $queries));

        $worlds = [];

        for ($i = 0; $i < $queries; ++$i) {
            $world = $this->getRandomWorld();
            $world->randomNumber = mt_rand(1, 10000);
            $world->save();
            $worlds[] = $world;
        }

        return $this->response->setJsonContent($worlds);
    }

    public function plaintextAction(): ResponseInterface
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

    protected function getFortunesArray(): array
    {
        // since the resultset is immutable get an array instead
        // so we can add the new fortune
        return Fortunes::find()->toArray();
    }

    protected function buildFortune()
    {
        return [
            'id' => 0,
            'message' => 'Additional fortune added at request time.'
        ];
    }

    protected function sortFortunes(array $fortunes): array
    {
        usort($fortunes, function ($left, $right) {
            return $left['message'] <=> $right['message'];
        });

        return $fortunes;
    }
}
