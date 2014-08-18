<?php

use Phalcon\Mvc\View,
    Phalcon\Mvc\Model\Resultset;

class MongobenchController extends BenchController
{

    protected function getRandomWorld()
    {
        return WorldsCollection::findFirst(array(array('_id' => mt_rand(1, 10000))));
    }

    protected function getFortunesArray()
    {
        return FortunesCollection::find();
    }

    protected function buildFortune()
    {
        $fortune = parent::buildFortune();
        $newFortune = new FortunesCollection();
        $newFortune->_id = $fortune['id'];
        $newFortune->message = $fortune['message'];
        return $newFortune;
    }

    protected function sortFortunes($fortunes)
    {
        usort($fortunes,
                function($left, $right) {
                    $l = $left->message;
                    $r = $right->message;
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
        return $fortunes;
    }

}
