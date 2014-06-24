<?php
/**
 * Created by JetBrains PhpStorm.
 * User: ocramius
 * Date: 4/28/13
 * Time: 9:29 PM
 * To change this template use File | Settings | File Templates.
 */

namespace FrameworkBenchmarks\ServiceFactory;

use FrameworkBenchmarks\Controller\DbController;
use FrameworkBenchmarks\Entity\World;
use Zend\Db\ResultSet\ResultSet;
use Zend\Db\TableGateway\TableGateway;
use Zend\ServiceManager\FactoryInterface;
use Zend\ServiceManager\ServiceLocatorInterface;

/**
 * Factory responsible for instantiating {@see \FrameworkBenchmarks\Controller\DbController}
 *
 * @author Marco Pivetta <ocramius@gmail.com>
 * @link   http://www.techempower.com/benchmarks
 */
class DbControllerServiceFactory implements FactoryInterface
{
    /**
     * {@inheritDoc}
     *
     * @return \FrameworkBenchmarks\Controller\DbController
     */
    public function createService(ServiceLocatorInterface $serviceLocator)
    {
        /* @var $serviceLocator \Zend\ServiceManager\AbstractPluginManager */
        /* @var $dbAdapter \Zend\Db\Adapter\AdapterInterface */
        $dbAdapter          = $serviceLocator->getServiceLocator()->get('Zend\Db\Adapter\Adapter');
        $resultSetPrototype = new ResultSet();

        $resultSetPrototype->setArrayObjectPrototype(new World());

        return new DbController(new TableGateway('World', $dbAdapter, null, $resultSetPrototype));
    }
}
