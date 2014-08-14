<?php

namespace Doctrine\Tests\Common\Annotations\Fixtures;

use Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetClass;
use Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAnnotation;

/**
 * @AnnotationTargetClass("Some data")
 */
class ClassWithInvalidAnnotationTargetAtProperty
{

    /**
     * @AnnotationTargetClass("Bar")
     */
    public $foo;


    /**
     * @AnnotationTargetAnnotation("Foo")
     */
    public $bar;
}