<?php

use Phalcon\Mvc\View,
    Phalcon\Mvc\Model\Resultset;

class MongobenchController extends BenchController
{

    protected function getRandomWorld()
    {
        return MongoWorldsCollection::findFirst(array(array('_id' => mt_rand(1, 10000))));
    }

    protected function getFortunesArray()
    {
        return MongoFortunesCollection::find();
    }

    protected function buildFortune()
    {
        $fortune = parent::buildFortune();
        $newFortune = new MongoFortunesCollection();
        $newFortune->_id = $fortune['id'];
        $newFortune->message = $fortune['message'];
        return $newFortune;
    }

    protected function sortFortunes($fortunes)
    {
        usort($fortunes,
                function($left, $right) {
                    return $left->message <=> $right->message;
                });
        return $fortunes;
    }

}
