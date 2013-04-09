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

use Symfony\Component\Routing\Route as BaseRoute;

/**
 * A wrapper for a controller, mapped to a route.
 *
 * @author Fabien Potencier <fabien@symfony.com>
 */
class Route extends BaseRoute
{
    public function __construct($pattern = '', array $defaults = array(), array $requirements = array(), array $options = array())
    {
        parent::__construct($pattern, $defaults, $requirements, $options);
    }

    /**
     * Sets the requirement for a route variable.
     *
     * @param string $variable The variable name
     * @param string $regexp   The regexp to apply
     *
     * @return Route $this The current route instance
     */
    public function assert($variable, $regexp)
    {
        $this->setRequirement($variable, $regexp);

        return $this;
    }

    /**
     * Sets the default value for a route variable.
     *
     * @param string $variable The variable name
     * @param mixed  $default  The default value
     *
     * @return Route $this The current Route instance
     */
    public function value($variable, $default)
    {
        $this->setDefault($variable, $default);

        return $this;
    }

    /**
     * Sets a converter for a route variable.
     *
     * @param string $variable The variable name
     * @param mixed  $callback A PHP callback that converts the original value
     *
     * @return Route $this The current Route instance
     */
    public function convert($variable, $callback)
    {
        $converters = $this->getOption('_converters');
        $converters[$variable] = $callback;
        $this->setOption('_converters', $converters);

        return $this;
    }

    /**
     * Sets the requirement for the HTTP method.
     *
     * @param string $method The HTTP method name. Multiple methods can be supplied, delimited by a pipe character '|', eg. 'GET|POST'
     *
     * @return Route $this The current Route instance
     */
    public function method($method)
    {
        $this->setRequirement('_method', $method);

        return $this;
    }

    /**
     * Sets the requirement of host on this Route.
     *
     * Note that this only works with at least version 2.2 of the Symfony Routing component.
     *
     * @param string $host The host for which this route should be enabled
     *
     * @return Route $this The current Route instance
     */
    public function host($host)
    {
        $this->setHost($host);

        return $this;
    }

    /**
     * Sets the requirement of HTTP (no HTTPS) on this Route.
     *
     * @return Route $this The current Route instance
     */
    public function requireHttp()
    {
        $this->setRequirement('_scheme', 'http');

        return $this;
    }

    /**
     * Sets the requirement of HTTPS on this Route.
     *
     * @return Route $this The current Route instance
     */
    public function requireHttps()
    {
        $this->setRequirement('_scheme', 'https');

        return $this;
    }

    /**
     * Sets a callback to handle before triggering the route callback.
     *
     * @param mixed $callback A PHP callback to be triggered when the Route is matched, just before the route callback
     *
     * @return Route $this The current Route instance
     */
    public function before($callback)
    {
        $callbacks = $this->getOption('_before_middlewares');
        $callbacks[] = $callback;
        $this->setOption('_before_middlewares', $callbacks);

        return $this;
    }

    /**
     * Sets a callback to handle after the route callback.
     *
     * @param mixed $callback A PHP callback to be triggered after the route callback
     *
     * @return Route $this The current Route instance
     */
    public function after($callback)
    {
        $callbacks = $this->getOption('_after_middlewares');
        $callbacks[] = $callback;
        $this->setOption('_after_middlewares', $callbacks);

        return $this;
    }
}
