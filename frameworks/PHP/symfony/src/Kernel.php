<?php

namespace App;

use App\Swoole\EntityManagerHandler;
use App\Swoole\SrandStartHandler;
use K911\Swoole\Server\RequestHandler\RequestHandlerInterface;
use K911\Swoole\Server\WorkerHandler\WorkerStartHandlerInterface;
use Symfony\Bundle\FrameworkBundle\Kernel\MicroKernelTrait;
use Symfony\Component\DependencyInjection\Compiler\CompilerPassInterface;
use Symfony\Component\DependencyInjection\ContainerBuilder;
use Symfony\Component\DependencyInjection\Loader\Configurator\ContainerConfigurator;
use Symfony\Component\DependencyInjection\Reference;
use Symfony\Component\HttpKernel\Kernel as BaseKernel;
use Symfony\Component\Routing\Loader\Configurator\RoutingConfigurator;

class Kernel extends BaseKernel implements CompilerPassInterface
{
    use MicroKernelTrait;

    protected function configureContainer(ContainerConfigurator $container): void
    {
        $container->parameters()->set('container.dumper.inline_factories', true);
        $container->parameters()->set('container.dumper.inline_class_loader', true);

        $container->import('../config/{packages}/*.yaml');
        $container->import('../config/{packages}/'.$this->environment.'/*.yaml');
        $container->import('../config/{packages}/'.$this->environment.'/*.php');
        $container->import('../config/{services}.yaml');
        $container->import('../config/{services}_'.$this->environment.'.yaml');
    }

    protected function configureRoutes(RoutingConfigurator $routes): void
    {
        $routes->import('../config/{routes}/'.$this->environment.'/*.yaml');
        $routes->import('../config/{routes}/*.yaml');
        $routes->import('../config/{routes}.yaml');
    }

    public function process(ContainerBuilder $container)
    {
        if ($this->environment !== 'swoole') {
            return;
        }

        $container->register(EntityManagerHandler::class, EntityManagerHandler::class)
            ->addArgument(new Reference(EntityManagerHandler::class.'.inner'))
            ->setAutowired(true)
            ->setAutoconfigured(true)
            ->setPublic(false)
            ->setDecoratedService(RequestHandlerInterface::class, null, -20)
        ;

        $container->register(SrandStartHandler::class, SrandStartHandler::class)
            ->addArgument(new Reference(SrandStartHandler::class.'.inner'))
            ->setAutowired(true)
            ->setAutoconfigured(true)
            ->setPublic(false)
            ->setDecoratedService(WorkerStartHandlerInterface::class)
        ;
    }
}
