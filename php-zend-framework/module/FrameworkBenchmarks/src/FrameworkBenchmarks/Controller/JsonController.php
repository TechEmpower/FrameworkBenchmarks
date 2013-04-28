<?php

namespace FrameworkBenchmarks\Controller;

use Zend\Mvc\Controller\AbstractActionController;
use Zend\View\Model\JsonModel;

/**
 * Controller that produces the `hello world` json for the benchmarks of FrameworkBenchmarks
 *
 * @author Marco Pivetta <ocramius@gmail.com>
 * @link   http://www.techempower.com/benchmarks
 */
class JsonController extends AbstractActionController
{
    /**
     * @return \Zend\View\Model\JsonModel
     */
    public function indexAction()
    {
        return new JsonModel(array('message' => 'Hello, World!'));
    }
}