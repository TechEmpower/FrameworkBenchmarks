<?php

class PlaintextController extends Zend_Controller_Action
{
    public function indexAction()
    {
      $this->_helper->viewRenderer->setNoRender(true);
      $this->getResponse()
           ->setHeader('Content-Type', 'text/plain')
           ->appendBody('Hello, World!');
    }
}
