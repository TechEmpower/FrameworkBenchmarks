<?php

class JsonController extends Zend_Controller_Action
{
    public function indexAction()
    {
        $this->_helper->json->sendJson(array('message' => 'Hello, World!'));
    }
}
