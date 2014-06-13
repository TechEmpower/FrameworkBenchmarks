<?php

class DbMultiController extends Zend_Controller_Action
{
    public function indexAction()
    {
        $queries = $this->getParam('queries', 1);

        $table = new Zend_Db_Table('World');

        $worlds = array();
        for ($i = 0; $i < $queries; $i += 1) {
            $worlds[] = $table->fetchRow(array('id = ?' => mt_rand(1,10000)))->toArray();
        }

        $this->_helper->json->sendJson($worlds);
    }
}

