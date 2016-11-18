<?php

class UpdatesController extends Zend_Controller_Action
{
    public function indexAction()
    {
        $queries = (int) $this->getParam('queries', 1);
        $queries = max(1, $queries);
        $queries = min(500, $queries);

        $table = new Model_World();

        $worlds = array();
        for ($i = 0; $i < $queries; $i += 1) {
            $id = mt_rand(1, 10000);
            $random_number = mt_rand(1, 10000);
            $world = $table->fetchRow(array('id = ?' => $id))->toArray();
            $world['randomNumber'] = $random_number;
            $where = $table->getAdapter()->quoteInto('id = ?', $id);
            $table->update(array('randomNumber' => $random_number), $where);
            $worlds[] = $world;
        }

        $this->_helper->json->sendJson($worlds);
    }
}
