<?php

namespace FrameworkBenchmarks;

/**
 * Module class for the benchmarks of FrameworkBenchmarks
 *
 * @author Marco Pivetta <ocramius@gmail.com>
 * @link   http://www.techempower.com/benchmarks
 */
class Module
{
    /**
     * @return array
     */
    public function getConfig()
    {
        return array(
            'router' => array(
                'routes' => array(
                    'json' => array(
                        'type' => 'Zend\Mvc\Router\Http\Literal',
                        'options' => array(
                            'route' => '/json',
                            'defaults' => array(
                                'controller' => 'FrameworkBenchmarks\Controller\JsonController',
                                'action' => 'index',
                            ),
                        ),
                    ),
                    'db' => array(
                        'type' => 'Zend\Mvc\Router\Http\Literal',
                        'options' => array(
                            'route' => '/db',
                            'defaults' => array(
                                'controller' => 'FrameworkBenchmarks\Controller\DbController',
                                'action' => 'db',
                            ),
                        ),
                    ),
                    'db-multi' => array(
                        'type' => 'Zend\Mvc\Router\Http\Literal',
                        'options' => array(
                            'route' => '/db-multi',
                            'defaults' => array(
                                'controller' => 'FrameworkBenchmarks\Controller\DbController',
                                'action' => 'db-multi',
                            ),
                        ),
                    ),
                ),
            ),
            'db' => array(
                'driver' => 'Pdo',
                'dsn'    => 'mysql:dbname=hello_world;host=localhost',
            ),
            'controllers' => array(
                'invokables' => array(
                    'FrameworkBenchmarks\Controller\JsonController' => 'FrameworkBenchmarks\Controller\JsonController',
                ),
                'factories' => array(
                    'FrameworkBenchmarks\Controller\DbController'
                        => 'FrameworkBenchmarks\ServiceFactory\DbControllerServiceFactory'
                ),
            ),
            'service_manager' => array(
                'factories' => array(
                    'Zend\Db\Adapter\Adapter' => 'Zend\Db\Adapter\AdapterServiceFactory',
                ),
            ),
        );
    }
}
