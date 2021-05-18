<?php

use Phalcon\Mvc\View;
use Phalcon\Mvc\Model\Resultset;

class MongoController extends BenchController
{
    protected function getRandomWorld()
    {
        return WorldsCollection::findFirst([
            'conditions' => [
                '_id' => mt_rand(1, 10000),
            ],
            'projection' => [
                //'id' => 0,
            ],
        ]);
    }

    protected function getFortunesArray(): array
    {
        return FortunesCollection::find()->toArray();
    }

    protected function buildFortune()
    {
        $fortune = parent::buildFortune();
        $newFortune = new FortunesCollection();
        $newFortune->setId($fortune['id']);
        $newFortune->message = $fortune['message'];

        return $newFortune;
    }

    protected function sortFortunes(array $fortunes): array
    {
        usort($fortunes, function ($left, $right) {
            return $left->message <=> $right->message;
        });

        return $fortunes;
    }
}
