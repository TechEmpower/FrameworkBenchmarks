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

use Symfony\Component\Routing\RequestContext as SymfonyRequestContext;
use Symfony\Component\Routing\Matcher\UrlMatcherInterface;

/**
 * Implements a lazy UrlMatcher.
 *
 * @author Igor Wiedler <igor@wiedler.ch>
 */
class LazyUrlMatcher implements UrlMatcherInterface
{
    private $factory;

    public function __construct(\Closure $factory)
    {
        $this->factory = $factory;
    }

    /**
     * Returns the corresponding UrlMatcherInterface instance.
     *
     * @return UrlMatcherInterface
     */
    public function getUrlMatcher()
    {
        $urlMatcher = call_user_func($this->factory);
        if (!$urlMatcher instanceof UrlMatcherInterface) {
            throw new \LogicException("Factory supplied to LazyUrlMatcher must return implementation of UrlMatcherInterface.");
        }

        return $urlMatcher;
    }

    /**
     * {@inheritdoc}
     */
    public function match($pathinfo)
    {
        return $this->getUrlMatcher()->match($pathinfo);
    }

    /**
     * {@inheritdoc}
     */
    public function setContext(SymfonyRequestContext $context)
    {
        $this->getUrlMatcher()->setContext($context);
    }

    /**
     * {@inheritdoc}
     */
    public function getContext()
    {
        return $this->getUrlMatcher()->getContext();
    }
}
