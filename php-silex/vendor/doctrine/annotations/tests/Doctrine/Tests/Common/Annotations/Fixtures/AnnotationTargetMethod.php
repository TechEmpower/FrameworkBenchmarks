<?php

namespace Doctrine\Tests\Common\Annotations\Fixtures;


/**
 * @Annotation
 * @Target("METHOD")
 */
final class AnnotationTargetMethod
{
    public $data;
    public $name;
    public $target;
}