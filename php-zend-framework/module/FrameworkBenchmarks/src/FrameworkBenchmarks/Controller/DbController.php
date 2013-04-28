<?php

namespace FrameworkBenchmarks\Controller;

use Zend\Mvc\Controller\AbstractActionController;
use Zend\View\Model\JsonModel;
use Zend\Db\TableGateway\TableGateway;

/**
 * Controller that produces the json for the DB benchmarks of FrameworkBenchmarks
 *
 * @author Marco Pivetta <ocramius@gmail.com>
 * @link   http://www.techempower.com/benchmarks
 */
class DbController extends AbstractActionController
{
    /**
     * @var \Zend\Db\TableGateway\TableGateway
     */
    protected $tableGateway;

    /**
     * @param TableGateway $tableGateway
     */
    public function __construct(TableGateway $tableGateway)
    {
        $this->tableGateway = $tableGateway;
    }

    /**
     * @return \Zend\View\Model\JsonModel
     */
    public function dbAction()
    {
        return new JsonModel($this->tableGateway->select(array('id' => mt_rand(1, 10000))));
    }

    /**
     * @return \Zend\View\Model\JsonModel
     */
    public function dbMultiAction()
    {
        /* @var $request \Zend\Http\Request */
        $request = $this->getRequest();
        $queries = $request->getQuery('queries', 1);
        $worlds  = array();

        for ($i = 0; $i < $queries; $i += 1) {
            $worlds[] =  $this->tableGateway->select(array('id' => mt_rand(1, 10000)));
        }

        return new JsonModel($worlds);
    }
}
