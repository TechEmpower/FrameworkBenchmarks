<?php

namespace Doctrine\Tests\Common\Annotations\Fixtures;

/**
 * @Annotation
 * @Target("ALL")
 */
class AnnotationTargetAll
{
    public $data;
    public $name;
    public $target;
}