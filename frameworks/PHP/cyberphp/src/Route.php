<?php
namespace Cyber;

use DI\Container;
use FastRoute;
use FastRoute\RouteCollector;

class Route
{
    public $dispatcher;
    // Route dispatch
    public function dispatcher($routes)
    {
        $this->dispatcher = FastRoute\simpleDispatcher(function (RouteCollector $r) use ($routes) {
            foreach ($routes as $route) {
                // Check the number of array members. Three members indicate a single route configuration.
                if (count($route) == 3) {
                    $r->addRoute(preg_split('/\s*,\s*/', $route[1]), $route[0], $route[2]);
                    // Two members indicate a group route.
                } elseif (count($route) == 2) {
                    $r->addGroup($route[0], function (RouteCollector $r) use ($route) {
                        foreach ($route[1] as $childRoute) {
                            $r->addRoute(preg_split('/\s*,\s*/', trim($childRoute[1])), $childRoute[0], $childRoute[2]);
                        }
                    });
                }
            }
        });
    }
    public function handleRoute()
    {
        $request = app()->request;
        $container = app()->container;
        // Parse the current route
        $routeInfo = $this->dispatcher->dispatch($request->getMethod(), $request->getPathInfo());
        if ($routeInfo[0] == 0) {
            throw new \Exception('Page not found', 404);
        } elseif ($routeInfo[0] == 2) {
            throw new \Exception('Request method error', 405);
        } elseif ($routeInfo[0] == 1) {
            $handler = $routeInfo[1];
            $vars = $routeInfo[2];
            $parameters = [...array_values($vars)];

            // Create a closure to pass to your middleware to execute the final handler
            $finalHandler = function() use ($handler, $parameters, $container) {
                // If handler is a string (controller@method)
                if (is_string($handler)) {
                    list($controller, $method) = explode('@', $handler);
                    $class = new $controller();
                    return $class->$method(...$parameters);
                } elseif (is_callable($handler)) {
                    return $handler(...$parameters);
                } else {
                    throw new \Exception('Route handler configuration error');
                }
            };
            return $finalHandler();
        }
    }
}