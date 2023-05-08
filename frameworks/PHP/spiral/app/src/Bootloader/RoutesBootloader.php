<?php

declare(strict_types=1);

namespace App\Bootloader;

use App\Controller\BenchmarkController;
use Spiral\Bootloader\Http\RoutesBootloader as BaseRoutesBootloader;
use Spiral\Router\Loader\Configurator\RoutingConfigurator;

final class RoutesBootloader extends BaseRoutesBootloader
{
    protected function globalMiddleware(): array
    {
        return [];
    }

    protected function middlewareGroups(): array
    {
        return [];
    }

    protected function defineRoutes(RoutingConfigurator $routes): void
    {
        $routes
            ->add('benchmark', '/<action>[/<queries>]')
            ->controller(BenchmarkController::class);
    }
}
