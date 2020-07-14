<?php
/**
 * Created by JetBrains PhpStorm.
 * User: ocramius
 * Date: 4/28/13
 * Time: 9:29 PM
 * To change this template use File | Settings | File Templates.
 */

namespace FrameworkBenchmarks\ServiceFactory;

use FrameworkBenchmarks\Controller\BenchController;
use FrameworkBenchmarks\Entity\World;
use Zend\Db\ResultSet\ResultSet;
use Zend\Db\TableGateway\TableGateway;

/**
 * Factory responsible for instantiating {@see \FrameworkBenchmarks\Controller\DbController}
 *
 * @author Marco Pivetta <ocramius@gmail.com>
 * @link   http://www.techempower.com/benchmarks
 */
class BenchControllerServiceFactory
{
    /**
     * {@inheritDoc}
     *
     * @return \FrameworkBenchmarks\Controller\DbController
     */
    public function __invoke(\Psr\Container\ContainerInterface $container)
    {
        /* @var $dbAdapter \Zend\Db\Adapter\AdapterInterface */
        $dbAdapter          = $container->get('Zend\Db\Adapter\Adapter');
        $resultSetPrototype = new ResultSet();

        $resultSetPrototype->setArrayObjectPrototype(new World());

        return new BenchController(new TableGateway('World', $dbAdapter, null, $resultSetPrototype));
    }
}
