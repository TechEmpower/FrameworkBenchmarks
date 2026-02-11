<?php

declare(strict_types=1);

use Hyperf\Testing\ModelFactory;
use Hypervel\Context\ApplicationContext;

if (! function_exists('factory')) {
    function factory(string $class, mixed ...$arguments)
    {
        return ApplicationContext::getContainer()
            ->get(ModelFactory::class)
            ->factory($class, ...$arguments);
    }
}
