<?php

/*
 * This file is part of the Silex framework.
 *
 * (c) Fabien Potencier <fabien@symfony.com>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

namespace Silex;

use Symfony\Component\Routing\RouteCollection;
use Silex\Controller;

/**
 * Builds Silex controllers.
 *
 * It acts as a staging area for routes. You are able to set the route name
 * until flush() is called, at which point all controllers are frozen and
 * converted to a RouteCollection.
 *
 * @author Igor Wiedler <igor@wiedler.ch>
 * @author Fabien Potencier <fabien@symfony.com>
 */
class ControllerCollection
{
    protected $controllers = array();
    protected $defaultRoute;

    /**
     * Constructor.
     */
    public function __construct(Route $defaultRoute)
    {
        $this->defaultRoute = $defaultRoute;
    }

    /**
     * Maps a pattern to a callable.
     *
     * You can optionally specify HTTP methods that should be matched.
     *
     * @param string $pattern Matched route pattern
     * @param mixed  $to      Callback that returns the response when matched
     *
     * @return Controller
     */
    public function match($pattern, $to)
    {
        $route = clone $this->defaultRoute;
        $route->setPattern($pattern);
        $route->setDefault('_controller', $to);

        $this->controllers[] = $controller = new Controller($route);

        return $controller;
    }

    /**
     * Maps a GET request to a callable.
     *
     * @param string $pattern Matched route pattern
     * @param mixed  $to      Callback that returns the response when matched
     *
     * @return Controller
     */
    public function get($pattern, $to)
    {
        return $this->match($pattern, $to)->method('GET');
    }

    /**
     * Maps a POST request to a callable.
     *
     * @param string $pattern Matched route pattern
     * @param mixed  $to      Callback that returns the response when matched
     *
     * @return Controller
     */
    public function post($pattern, $to)
    {
        return $this->match($pattern, $to)->method('POST');
    }

    /**
     * Maps a PUT request to a callable.
     *
     * @param string $pattern Matched route pattern
     * @param mixed  $to      Callback that returns the response when matched
     *
     * @return Controller
     */
    public function put($pattern, $to)
    {
        return $this->match($pattern, $to)->method('PUT');
    }

    /**
     * Maps a DELETE request to a callable.
     *
     * @param string $pattern Matched route pattern
     * @param mixed  $to      Callback that returns the response when matched
     *
     * @return Controller
     */
    public function delete($pattern, $to)
    {
        return $this->match($pattern, $to)->method('DELETE');
    }

    public function __call($method, $arguments)
    {
        if (!method_exists($this->defaultRoute, $method)) {
            throw new \BadMethodCallException(sprintf('Method "%s::%s" does not exist.', get_class($this->defaultRoute), $method));
        }

        call_user_func_array(array($this->defaultRoute, $method), $arguments);

        foreach ($this->controllers as $controller) {
            call_user_func_array(array($controller, $method), $arguments);
        }

        return $this;
    }

    /**
     * Persists and freezes staged controllers.
     *
     * @param string $prefix
     *
     * @return RouteCollection A RouteCollection instance
     */
    public function flush($prefix = '')
    {
        $routes = new RouteCollection();

        foreach ($this->controllers as $controller) {
            if (!$name = $controller->getRouteName()) {
                $name = $controller->generateRouteName($prefix);
                while ($routes->get($name)) {
                    $name .= '_';
                }
                $controller->bind($name);
            }
            $routes->add($name, $controller->getRoute());
            $controller->freeze();
        }

        $routes->addPrefix($prefix);

        $this->controllers = array();

        return $routes;
    }
}
