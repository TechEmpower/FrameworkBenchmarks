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

use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\HttpKernel\Exception\HttpExceptionInterface;
use Symfony\Component\HttpKernel\KernelEvents;
use Symfony\Component\HttpKernel\Event\GetResponseForExceptionEvent;
use Symfony\Component\HttpKernel\Event\GetResponseForControllerResultEvent;
use Silex\Application;

/**
 * Wraps exception listeners.
 *
 * @author Fabien Potencier <fabien@symfony.com>
 */
class ExceptionListenerWrapper
{
    protected $app;
    protected $callback;

    /**
     * Constructor.
     *
     * @param Application $app An Application instance
     */
    public function __construct(Application $app, $callback)
    {
        $this->app = $app;
        $this->callback = $callback;
    }

    public function __invoke(GetResponseForExceptionEvent $event)
    {
        $exception = $event->getException();

        if (!$this->shouldRun($exception)) {
            return;
        }

        $code = $exception instanceof HttpExceptionInterface ? $exception->getStatusCode() : 500;

        $response = call_user_func($this->callback, $exception, $code);

        $this->ensureResponse($response, $event);
    }

    protected function shouldRun(\Exception $exception)
    {
        if (is_array($this->callback)) {
            $callbackReflection = new \ReflectionMethod($this->callback[0], $this->callback[1]);
        } elseif (is_object($this->callback) && !$this->callback instanceof \Closure) {
            $callbackReflection = new \ReflectionObject($this->callback);
            $callbackReflection = $callbackReflection->getMethod('__invoke');
        } else {
            $callbackReflection = new \ReflectionFunction($this->callback);
        }

        if ($callbackReflection->getNumberOfParameters() > 0) {
            $parameters = $callbackReflection->getParameters();
            $expectedException = $parameters[0];
            if ($expectedException->getClass() && !$expectedException->getClass()->isInstance($exception)) {
                return false;
            }
        }

        return true;
    }

    protected function ensureResponse($response, GetResponseForExceptionEvent $event)
    {
        if ($response instanceof Response) {
            $event->setResponse($response);
        } else {
            $viewEvent = new GetResponseForControllerResultEvent($this->app['kernel'], $event->getRequest(), $event->getRequestType(), $response);
            $this->app['dispatcher']->dispatch(KernelEvents::VIEW, $viewEvent);

            if ($viewEvent->hasResponse()) {
                $event->setResponse($viewEvent->getResponse());
            }
        }
    }
}
