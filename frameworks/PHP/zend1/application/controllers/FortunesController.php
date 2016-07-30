<?php

class FortunesController extends Zend_Controller_Action
{
    public function indexAction()
    {
        $table = new Model_Fortune();
        $fortunes = $table->fetchAll()->toArray();
        array_push($fortunes, array('id'=> 0, 'message' => 'Additional fortune added at request time.'));
        usort($fortunes, function($left, $right) {
            return strcmp($left['message'], $right['message']);
        });
        $this->view->fortunes = $fortunes;
    }
}
