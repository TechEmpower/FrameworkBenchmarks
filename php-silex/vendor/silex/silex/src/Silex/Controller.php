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

use Silex\Exception\ControllerFrozenException;

/**
 * A wrapper for a controller, mapped to a route.
 *
 * __call() forwards method-calls to Route, but returns instance of Controller
 * listing Route's methods below, so that IDEs know they are valid
 *
 * @method \Silex\Controller assert(string $variable, string $regexp)
 * @method \Silex\Controller value(string $variable, mixed $default)
 * @method \Silex\Controller convert(string $variable, mixed $callback)
 * @method \Silex\Controller method(string $method)
 * @method \Silex\Controller requireHttp()
 * @method \Silex\Controller requireHttps()
 * @method \Silex\Controller before(mixed $callback)
 * @method \Silex\Controller after(mixed $callback)
 * @author Igor Wiedler <igor@wiedler.ch>
 */
class Controller
{
    private $route;
    private $routeName;
    private $isFrozen = false;

    /**
     * Constructor.
     *
     * @param Route $route
     */
    public function __construct(Route $route)
    {
        $this->route = $route;
    }

    /**
     * Gets the controller's route.
     *
     * @return Route
     */
    public function getRoute()
    {
        return $this->route;
    }

    /**
     * Gets the controller's route name.
     *
     * @return string
     */
    public function getRouteName()
    {
        return $this->routeName;
    }

    /**
     * Sets the controller's route.
     *
     * @param string $routeName
     *
     * @return Controller $this The current Controller instance
     */
    public function bind($routeName)
    {
        if ($this->isFrozen) {
            throw new ControllerFrozenException(sprintf('Calling %s on frozen %s instance.', __METHOD__, __CLASS__));
        }

        $this->routeName = $routeName;

        return $this;
    }

    public function __call($method, $arguments)
    {
        if (!method_exists($this->route, $method)) {
            throw new \BadMethodCallException(sprintf('Method "%s::%s" does not exist.', get_class($this->route), $method));
        }

        call_user_func_array(array($this->route, $method), $arguments);

        return $this;
    }

    /**
     * Freezes the controller.
     *
     * Once the controller is frozen, you can no longer change the route name
     */
    public function freeze()
    {
        $this->isFrozen = true;
    }

    public function generateRouteName($prefix)
    {
        $requirements = $this->route->getRequirements();
        $method = isset($requirements['_method']) ? $requirements['_method'] : '';

        $routeName = $prefix.$method.$this->route->getPattern();
        $routeName = str_replace(array('/', ':', '|', '-'), '_', $routeName);
        $routeName = preg_replace('/[^a-z0-9A-Z_.]+/', '', $routeName);

        return $routeName;
    }
}
