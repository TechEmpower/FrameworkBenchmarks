<?php

namespace Doctrine\Tests\Common\Reflection;

use Doctrine\Tests\DoctrineTestCase;
use Doctrine\Common\Reflection\StaticReflectionParser;
use Doctrine\Common\Reflection\Psr0FindFile;

class StaticReflectionParserTest extends DoctrineTestCase
{
    /**
     * @dataProvider classAnnotationOptimize
     *
     * @param bool $classAnnotationOptimize
     *
     * @return void
     */
    public function testParentClass($classAnnotationOptimize)
    {
        $testsRoot = substr(__DIR__, 0, -strlen(__NAMESPACE__) - 1);
        $paths = array(
            'Doctrine\\Tests' => array($testsRoot),
        );
        $noParentClassName = 'Doctrine\\Tests\\Common\\Reflection\\NoParent';
        $staticReflectionParser = new StaticReflectionParser($noParentClassName, new Psr0FindFile($paths), $classAnnotationOptimize);
        $declaringClassName = $staticReflectionParser->getStaticReflectionParserForDeclaringClass('property', 'test')->getClassName();
        $this->assertEquals($noParentClassName, $declaringClassName);

        $className = 'Doctrine\\Tests\\Common\\Reflection\\FullyClassifiedParent';
        $staticReflectionParser = new StaticReflectionParser($className, new Psr0FindFile($paths), $classAnnotationOptimize);
        $declaringClassName = $staticReflectionParser->getStaticReflectionParserForDeclaringClass('property', 'test')->getClassName();
        $this->assertEquals($noParentClassName, $declaringClassName);

        $className = 'Doctrine\\Tests\\Common\\Reflection\\SameNamespaceParent';
        $staticReflectionParser = new StaticReflectionParser($className, new Psr0FindFile($paths), $classAnnotationOptimize);
        $declaringClassName = $staticReflectionParser->getStaticReflectionParserForDeclaringClass('property', 'test')->getClassName();
        $this->assertEquals($noParentClassName, $declaringClassName);

        $dummyParentClassName = 'Doctrine\\Tests\\Common\\Reflection\\Dummies\\NoParent';

        $className = 'Doctrine\\Tests\\Common\\Reflection\\DeeperNamespaceParent';
        $staticReflectionParser = new StaticReflectionParser($className, new Psr0FindFile($paths), $classAnnotationOptimize);
        $declaringClassName = $staticReflectionParser->getStaticReflectionParserForDeclaringClass('property', 'test')->getClassName();
        $this->assertEquals($dummyParentClassName, $declaringClassName);

        $className = 'Doctrine\\Tests\\Common\\Reflection\\UseParent';
        $staticReflectionParser = new StaticReflectionParser($className, new Psr0FindFile($paths), $classAnnotationOptimize);
        $declaringClassName = $staticReflectionParser->getStaticReflectionParserForDeclaringClass('property', 'test')->getClassName();
        $this->assertEquals($dummyParentClassName, $declaringClassName);

    }

    /**
     * @return array
     */
    public function classAnnotationOptimize()
    {
        return array(
            array(false),
            array(true)
        );
    }
}
