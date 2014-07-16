<?php

namespace Doctrine\Tests\Common\Annotations\Fixtures\Annotation;

/** @Annotation */
class Route
{
    /** @var string @Required */
    public $pattern;
    public $name;
}