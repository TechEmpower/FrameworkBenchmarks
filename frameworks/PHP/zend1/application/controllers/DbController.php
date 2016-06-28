<?php

class DbController extends Zend_Controller_Action
{
    public function indexAction()
    {
        $table = new Model_World();
        $result = $table->fetchRow(array('id = ?' => mt_rand(1, 10000)));

        $this->_helper->json->sendJson($result->toArray());
    }
}
