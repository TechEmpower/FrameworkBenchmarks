<?php

namespace FrameworkBenchmarks\Controller;

use Zend\Mvc\Controller\AbstractActionController;
use Zend\Stdlib\ArrayUtils;
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
        $result = $this->tableGateway->select(array('id' => mt_rand(1, 10000)));

        foreach ($result as $return) {
            return new JsonModel($return);
        }

        return $this->notFoundAction();
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
            foreach ($this->tableGateway->select(array('id' => mt_rand(1, 10000))) as $found) {
                $worlds[] = $found;
            }
        }

        return new JsonModel($worlds);
    }
}
