<?php

class DbMultiController extends Zend_Controller_Action
{
    public function indexAction()
    {
        $queries = (int) $this->getParam('queries', 1);
        $queries = max(1, $queries);
        $queries = min(500, $queries);

        $table = new Zend_Db_Table('World');

        $worlds = array();
        for ($i = 0; $i < $queries; $i += 1) {
            $worlds[] = $table->fetchRow(array('id = ?' => mt_rand(1,10000)))->toArray();
        }

        $this->_helper->json->sendJson($worlds);
    }
}
