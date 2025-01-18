<?php
namespace Cyber;

class Middleware
{

    public function handleRequest(array $requestMiddlewares)
    {
        $request = app()->request;
        foreach ($requestMiddlewares as $middleware) {
            if (!class_exists($middleware)) {
                throw new \Exception("The parameter class {$middleware} for processing the request middleware does not exist");
            }
            $instance = app()->container->get($middleware);
            if (!method_exists($instance, 'handle')) {
                throw new \Exception("The parameter class {$middleware} for processing the request middleware does not have a handle method");
            }
            /* Call the handle method of the request data middleware */
            $request = $instance->handle($request);
        }
        return $request;
    }

    public function handle(array $Middlewares, callable $finalHandler)
    {
        $request = app()->request;
        $container = app()->container;
        // Start wrapping the handler from the last middleware layer by layer
        $response = array_reduce(
            array_reverse($Middlewares),
            function($next, $middleware) use ($request,$container) {
                if (!class_exists($middleware)) {
                    throw new \Exception("The middleware parameter class {$middleware} does not exist");
                }
                $instance = $container->get($middleware);
                if (!method_exists($instance, 'handle')) {
                    throw new \Exception("The middleware parameter class {$middleware} does not have a handle method");
                }
                return function() use ($instance, $request, $next) {
                    return $instance->handle($request, $next);
                };
            },
            $finalHandler
        );
        // Execute the middleware chain
        return $response();
    }
}