<?php

use Yaf\Application;
use Yaf\Bootstrap_Abstract as AbstractBootstrap;
use Yaf\Dispatcher;
use eYaf\Layout;

class Bootstrap extends AbstractBootstrap
{
    /**
     * @var Yaf\Config_Abstract
     */
    private $config = null;

    public function _initConfig ()
    {
        $this->config = Application::app()->getConfig();
    }

    public function _initLayout (Dispatcher $dispatcher)
    {
        $dispatcher->setView(new Layout($this->config->get('application.layout.directory')));
    }

}
