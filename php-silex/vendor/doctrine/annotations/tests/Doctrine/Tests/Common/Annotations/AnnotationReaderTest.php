<?php

namespace Doctrine\Tests\Common\Annotations;

use Doctrine\Common\Annotations\AnnotationReader;

class AnnotationReaderTest extends AbstractReaderTest
{
    protected function getReader()
    {
        return new AnnotationReader();
    }
}