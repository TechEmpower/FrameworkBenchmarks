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

use Symfony\Component\HttpKernel\Debug\ExceptionHandler as DebugExceptionHandler;
use Symfony\Component\EventDispatcher\EventSubscriberInterface;
use Symfony\Component\HttpKernel\Event\GetResponseForExceptionEvent;
use Symfony\Component\HttpKernel\KernelEvents;

/**
 * Defaults exception handler.
 *
 * @author Fabien Potencier <fabien@symfony.com>
 */
class ExceptionHandler implements EventSubscriberInterface
{
    protected $debug;
    protected $enabled;

    public function __construct($debug)
    {
        $this->debug = $debug;
        $this->enabled = true;
    }

    public function disable()
    {
        $this->enabled = false;
    }

    public function onSilexError(GetResponseForExceptionEvent $event)
    {
        if (!$this->enabled) {
            return;
        }

        $handler = new DebugExceptionHandler($this->debug);

        $event->setResponse($handler->createResponse($event->getException()));
    }

    /**
     * {@inheritdoc}
     */
    public static function getSubscribedEvents()
    {
        return array(KernelEvents::EXCEPTION => array('onSilexError', -255));
    }
}
