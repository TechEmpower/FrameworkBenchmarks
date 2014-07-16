<?php

namespace Doctrine\Tests\Common\Annotations\Fixtures;

// Include a class named Api
require_once(__DIR__ . '/Api.php');

use Doctrine\Tests\Common\Annotations\DummyAnnotationWithIgnoredAnnotation;

/**
 * @DummyAnnotationWithIgnoredAnnotation(dummyValue="hello")
 */
class ClassWithRequire
{
}