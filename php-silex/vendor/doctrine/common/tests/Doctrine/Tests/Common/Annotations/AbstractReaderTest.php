<?php

namespace Doctrine\Tests\Common\Annotations;

use Doctrine\Common\Annotations\DoctrineReader;
use Doctrine\Common\Reflection\StaticReflectionParser;
use Doctrine\Common\Reflection\Psr0FindFile;
use Doctrine\Common\Annotations\Annotation\IgnoreAnnotation;
use Doctrine\Common\Annotations\Annotation\IgnorePhpDoc;
use ReflectionClass, Doctrine\Common\Annotations\AnnotationReader;

use Doctrine\Tests\Common\Annotations\DummyAnnotation;
use Doctrine\Tests\Common\Annotations\Name;
use Doctrine\Tests\Common\Annotations\DummyId;
use Doctrine\Tests\Common\Annotations\DummyJoinTable;
use Doctrine\Tests\Common\Annotations\DummyJoinColumn;
use Doctrine\Tests\Common\Annotations\DummyColumn;
use Doctrine\Tests\Common\Annotations\DummyGeneratedValue;

require_once __DIR__ . '/TopLevelAnnotation.php';

abstract class AbstractReaderTest extends \PHPUnit_Framework_TestCase
{
    public function getReflectionClass()
    {
        $className = 'Doctrine\Tests\Common\Annotations\DummyClass';
        $testsRoot = substr(__DIR__, 0, -strlen(__NAMESPACE__) - 1);
        $paths = array(
            'Doctrine\\Tests' => array($testsRoot),
        );
        $staticReflectionParser = new StaticReflectionParser($className, new Psr0FindFile($paths));
        return array(
            'native' => array(new ReflectionClass($className)),
            'static' => array($staticReflectionParser->getReflectionClass()),
        );
    }

    /**
     * @dataProvider getReflectionClass
     */
    public function testAnnotations($class)
    {
        $reader = $this->getReader();
        $this->assertEquals(1, count($reader->getClassAnnotations($class)));
        $this->assertInstanceOf($annotName = 'Doctrine\Tests\Common\Annotations\DummyAnnotation', $annot = $reader->getClassAnnotation($class, $annotName));
        $this->assertEquals("hello", $annot->dummyValue);

        $field1Prop = $class->getProperty('field1');
        $propAnnots = $reader->getPropertyAnnotations($field1Prop);
        $this->assertEquals(1, count($propAnnots));
        $this->assertInstanceOf($annotName, $annot = $reader->getPropertyAnnotation($field1Prop, $annotName));
        $this->assertEquals("fieldHello", $annot->dummyValue);

        $getField1Method = $class->getMethod('getField1');
        $methodAnnots = $reader->getMethodAnnotations($getField1Method);
        $this->assertEquals(1, count($methodAnnots));
        $this->assertInstanceOf($annotName, $annot = $reader->getMethodAnnotation($getField1Method, $annotName));
        $this->assertEquals(array(1, 2, "three"), $annot->value);

        $field2Prop = $class->getProperty('field2');
        $propAnnots = $reader->getPropertyAnnotations($field2Prop);
        $this->assertEquals(1, count($propAnnots));
        $this->assertInstanceOf($annotName = 'Doctrine\Tests\Common\Annotations\DummyJoinTable', $joinTableAnnot = $reader->getPropertyAnnotation($field2Prop, $annotName));
        $this->assertEquals(1, count($joinTableAnnot->joinColumns));
        $this->assertEquals(1, count($joinTableAnnot->inverseJoinColumns));
        $this->assertTrue($joinTableAnnot->joinColumns[0] instanceof DummyJoinColumn);
        $this->assertTrue($joinTableAnnot->inverseJoinColumns[0] instanceof DummyJoinColumn);
        $this->assertEquals('col1', $joinTableAnnot->joinColumns[0]->name);
        $this->assertEquals('col2', $joinTableAnnot->joinColumns[0]->referencedColumnName);
        $this->assertEquals('col3', $joinTableAnnot->inverseJoinColumns[0]->name);
        $this->assertEquals('col4', $joinTableAnnot->inverseJoinColumns[0]->referencedColumnName);

        $dummyAnnot = $reader->getMethodAnnotation($class->getMethod('getField1'), 'Doctrine\Tests\Common\Annotations\DummyAnnotation');
        $this->assertEquals('', $dummyAnnot->dummyValue);
        $this->assertEquals(array(1, 2, 'three'), $dummyAnnot->value);

        $dummyAnnot = $reader->getPropertyAnnotation($class->getProperty('field1'), 'Doctrine\Tests\Common\Annotations\DummyAnnotation');
        $this->assertEquals('fieldHello', $dummyAnnot->dummyValue);

        $classAnnot = $reader->getClassAnnotation($class, 'Doctrine\Tests\Common\Annotations\DummyAnnotation');
        $this->assertEquals('hello', $classAnnot->dummyValue);
    }

    public function testAnnotationsWithValidTargets()
    {
        $reader = $this->getReader();
        $class  = new ReflectionClass('Doctrine\Tests\Common\Annotations\Fixtures\ClassWithValidAnnotationTarget');

        $this->assertEquals(1,count($reader->getClassAnnotations($class)));
        $this->assertEquals(1,count($reader->getPropertyAnnotations($class->getProperty('foo'))));
        $this->assertEquals(1,count($reader->getMethodAnnotations($class->getMethod('someFunction'))));
        $this->assertEquals(1,count($reader->getPropertyAnnotations($class->getProperty('nested'))));
    }

    public function testAnnotationsWithVarType()
    {
        $reader = $this->getReader();
        $class  = new ReflectionClass('Doctrine\Tests\Common\Annotations\Fixtures\ClassWithAnnotationWithVarType');

        $this->assertEquals(1,count($fooAnnot = $reader->getPropertyAnnotations($class->getProperty('foo'))));
        $this->assertEquals(1,count($barAnnot = $reader->getMethodAnnotations($class->getMethod('bar'))));

        $this->assertInternalType('string',  $fooAnnot[0]->string);
        $this->assertInstanceOf('Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll', $barAnnot[0]->annotation);
    }

     /**
     * @expectedException Doctrine\Common\Annotations\AnnotationException
     * @expectedExceptionMessage [Semantical Error] Annotation @AnnotationTargetPropertyMethod is not allowed to be declared on class Doctrine\Tests\Common\Annotations\Fixtures\ClassWithInvalidAnnotationTargetAtClass. You may only use this annotation on these code elements: METHOD, PROPERTY
     */
    public function testClassWithInvalidAnnotationTargetAtClassDocBlock()
    {
        $reader  = $this->getReader();
        $reader->getClassAnnotations(new \ReflectionClass('Doctrine\Tests\Common\Annotations\Fixtures\ClassWithInvalidAnnotationTargetAtClass'));
    }

     /**
     * @expectedException Doctrine\Common\Annotations\AnnotationException
     * @expectedExceptionMessage [Semantical Error] Annotation @AnnotationTargetClass is not allowed to be declared on property Doctrine\Tests\Common\Annotations\Fixtures\ClassWithInvalidAnnotationTargetAtProperty::$foo. You may only use this annotation on these code elements: CLASS
     */
    public function testClassWithInvalidAnnotationTargetAtPropertyDocBlock()
    {
        $reader  = $this->getReader();
        $reader->getPropertyAnnotations(new \ReflectionProperty('Doctrine\Tests\Common\Annotations\Fixtures\ClassWithInvalidAnnotationTargetAtProperty', 'foo'));
    }

     /**
     * @expectedException Doctrine\Common\Annotations\AnnotationException
     * @expectedExceptionMessage [Semantical Error] Annotation @AnnotationTargetAnnotation is not allowed to be declared on property Doctrine\Tests\Common\Annotations\Fixtures\ClassWithInvalidAnnotationTargetAtProperty::$bar. You may only use this annotation on these code elements: ANNOTATION
     */
    public function testClassWithInvalidNestedAnnotationTargetAtPropertyDocBlock()
    {
        $reader  = $this->getReader();
        $reader->getPropertyAnnotations(new \ReflectionProperty('Doctrine\Tests\Common\Annotations\Fixtures\ClassWithInvalidAnnotationTargetAtProperty', 'bar'));
    }

     /**
     * @expectedException Doctrine\Common\Annotations\AnnotationException
     * @expectedExceptionMessage [Semantical Error] Annotation @AnnotationTargetClass is not allowed to be declared on method Doctrine\Tests\Common\Annotations\Fixtures\ClassWithInvalidAnnotationTargetAtMethod::functionName(). You may only use this annotation on these code elements: CLASS
     */
    public function testClassWithInvalidAnnotationTargetAtMethodDocBlock()
    {
        $reader  = $this->getReader();
        $reader->getMethodAnnotations(new \ReflectionMethod('Doctrine\Tests\Common\Annotations\Fixtures\ClassWithInvalidAnnotationTargetAtMethod', 'functionName'));
    }

    /**
     * @expectedException Doctrine\Common\Annotations\AnnotationException
     * @expectedExceptionMessage Expected namespace separator or identifier, got ')' at position 24 in class @Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithTargetSyntaxError.
     */
    public function testClassWithAnnotationWithTargetSyntaxErrorAtClassDocBlock()
    {
        $reader  = $this->getReader();
        $reader->getClassAnnotations(new \ReflectionClass('Doctrine\Tests\Common\Annotations\Fixtures\ClassWithAnnotationWithTargetSyntaxError'));
    }

    /**
     * @expectedException Doctrine\Common\Annotations\AnnotationException
     * @expectedExceptionMessage Expected namespace separator or identifier, got ')' at position 24 in class @Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithTargetSyntaxError.
     */
    public function testClassWithAnnotationWithTargetSyntaxErrorAtPropertyDocBlock()
    {
        $reader  = $this->getReader();
        $reader->getPropertyAnnotations(new \ReflectionProperty('Doctrine\Tests\Common\Annotations\Fixtures\ClassWithAnnotationWithTargetSyntaxError','foo'));
    }

    /**
     * @expectedException Doctrine\Common\Annotations\AnnotationException
     * @expectedExceptionMessage Expected namespace separator or identifier, got ')' at position 24 in class @Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithTargetSyntaxError.
     */
    public function testClassWithAnnotationWithTargetSyntaxErrorAtMethodDocBlock()
    {
        $reader  = $this->getReader();
        $reader->getMethodAnnotations(new \ReflectionMethod('Doctrine\Tests\Common\Annotations\Fixtures\ClassWithAnnotationWithTargetSyntaxError','bar'));
    }

    /**
     * @expectedException Doctrine\Common\Annotations\AnnotationException
     * @expectedExceptionMessage [Type Error] Attribute "string" of @AnnotationWithVarType declared on property Doctrine\Tests\Common\Annotations\Fixtures\ClassWithAnnotationWithVarType::$invalidProperty expects a(n) string, but got integer.
     */
    public function testClassWithPropertyInvalidVarTypeError()
    {
        $reader = $this->getReader();
        $class  = new ReflectionClass('Doctrine\Tests\Common\Annotations\Fixtures\ClassWithAnnotationWithVarType');

        $reader->getPropertyAnnotations($class->getProperty('invalidProperty'));
    }

    /**
     * @expectedException Doctrine\Common\Annotations\AnnotationException
     * @expectedExceptionMessage [Type Error] Attribute "annotation" of @AnnotationWithVarType declared on method Doctrine\Tests\Common\Annotations\Fixtures\ClassWithAnnotationWithVarType::invalidMethod() expects a(n) Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll, but got an instance of Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAnnotation.
     */
    public function testClassWithMethodInvalidVarTypeError()
    {
        $reader = $this->getReader();
        $class  = new ReflectionClass('Doctrine\Tests\Common\Annotations\Fixtures\ClassWithAnnotationWithVarType');

        $reader->getMethodAnnotations($class->getMethod('invalidMethod'));
    }

    /**
     * @expectedException Doctrine\Common\Annotations\AnnotationException
     * @expectedExceptionMessage Expected namespace separator or identifier, got ')' at position 18 in class Doctrine\Tests\Common\Annotations\DummyClassSyntaxError.
     */
    public function testClassSyntaxErrorContext()
    {
        $reader = $this->getReader();
        $reader->getClassAnnotations(new \ReflectionClass('Doctrine\Tests\Common\Annotations\DummyClassSyntaxError'));
    }

    /**
     * @expectedException Doctrine\Common\Annotations\AnnotationException
     * @expectedExceptionMessage Expected namespace separator or identifier, got ')' at position 18 in method Doctrine\Tests\Common\Annotations\DummyClassMethodSyntaxError::foo().
     */
    public function testMethodSyntaxErrorContext()
    {
        $reader = $this->getReader();
        $reader->getMethodAnnotations(new \ReflectionMethod('Doctrine\Tests\Common\Annotations\DummyClassMethodSyntaxError', 'foo'));
    }

    /**
     * @expectedException Doctrine\Common\Annotations\AnnotationException
     * @expectedExceptionMessage Expected namespace separator or identifier, got ')' at position 18 in property Doctrine\Tests\Common\Annotations\DummyClassPropertySyntaxError::$foo.
     */
    public function testPropertySyntaxErrorContext()
    {
        $reader = $this->getReader();
        $reader->getPropertyAnnotations(new \ReflectionProperty('Doctrine\Tests\Common\Annotations\DummyClassPropertySyntaxError', 'foo'));
    }

    /**
     * @group regression
     */
    public function testMultipleAnnotationsOnSameLine()
    {
        $reader = $this->getReader();
        $annots = $reader->getPropertyAnnotations(new \ReflectionProperty('Doctrine\Tests\Common\Annotations\DummyClass2', 'id'));
        $this->assertEquals(3, count($annots));
    }

    public function testNonAnnotationProblem()
    {
        $reader = $this->getReader();

        $this->assertNotNull($annot = $reader->getPropertyAnnotation(new \ReflectionProperty('Doctrine\Tests\Common\Annotations\DummyClassNonAnnotationProblem', 'foo'), $name = 'Doctrine\Tests\Common\Annotations\DummyAnnotation'));
        $this->assertInstanceOf($name, $annot);
    }

    public function testImportWithConcreteAnnotation()
    {
        $reader = $this->getReader();
        $property = new \ReflectionProperty('Doctrine\Tests\Common\Annotations\TestImportWithConcreteAnnotation', 'field');
        $annotations = $reader->getPropertyAnnotations($property);
        $this->assertEquals(1, count($annotations));
        $this->assertNotNull($reader->getPropertyAnnotation($property, 'Doctrine\Tests\Common\Annotations\DummyAnnotation'));
    }

    public function testImportWithInheritance()
    {
        $reader = $this->getReader();

        $class = new TestParentClass();
        $ref = new \ReflectionClass($class);

        $childAnnotations = $reader->getPropertyAnnotations($ref->getProperty('child'));
        $this->assertEquals(1, count($childAnnotations));
        $this->assertInstanceOf('Doctrine\Tests\Common\Annotations\Foo\Name', reset($childAnnotations));

        $parentAnnotations = $reader->getPropertyAnnotations($ref->getProperty('parent'));
        $this->assertEquals(1, count($parentAnnotations));
        $this->assertInstanceOf('Doctrine\Tests\Common\Annotations\Bar\Name', reset($parentAnnotations));
    }

    /**
     * @expectedException Doctrine\Common\Annotations\AnnotationException
     * @expectedExceptionMessage The annotation "@NameFoo" in property Doctrine\Tests\Common\Annotations\TestAnnotationNotImportedClass::$field was never imported.
     */
    public function testImportDetectsNotImportedAnnotation()
    {
        $reader = $this->getReader();
        $reader->getPropertyAnnotations(new \ReflectionProperty('Doctrine\Tests\Common\Annotations\TestAnnotationNotImportedClass', 'field'));
    }

    /**
     * @expectedException Doctrine\Common\Annotations\AnnotationException
     * @expectedExceptionMessage The annotation "@Foo\Bar\Name" in property Doctrine\Tests\Common\Annotations\TestNonExistentAnnotationClass::$field was never imported.
     */
    public function testImportDetectsNonExistentAnnotation()
    {
        $reader = $this->getReader();
        $reader->getPropertyAnnotations(new \ReflectionProperty('Doctrine\Tests\Common\Annotations\TestNonExistentAnnotationClass', 'field'));
    }

    public function testTopLevelAnnotation()
    {
        $reader = $this->getReader();
        $annotations = $reader->getPropertyAnnotations(new \ReflectionProperty('Doctrine\Tests\Common\Annotations\TestTopLevelAnnotationClass', 'field'));

        $this->assertEquals(1, count($annotations));
        $this->assertInstanceOf('\TopLevelAnnotation', reset($annotations));
    }

    public function testIgnoresAnnotationsNotPrefixedWithWhitespace()
    {
        $reader = $this->getReader();

        $annotation = $reader->getClassAnnotation(new \ReflectionClass(new TestIgnoresNonAnnotationsClass()), 'Doctrine\Tests\Common\Annotations\Name');
        $this->assertInstanceOf('Doctrine\Tests\Common\Annotations\Name', $annotation);
    }

    /**
     * @expectedException Doctrine\Common\Annotations\AnnotationException
     * @expectedExceptionMessage The class "Doctrine\Tests\Common\Annotations\Fixtures\NoAnnotation" is not annotated with @Annotation. Are you sure this class can be used as annotation? If so, then you need to add @Annotation to the _class_ doc comment of "Doctrine\Tests\Common\Annotations\Fixtures\NoAnnotation". If it is indeed no annotation, then you need to add @IgnoreAnnotation("NoAnnotation") to the _class_ doc comment of class Doctrine\Tests\Common\Annotations\Fixtures\InvalidAnnotationUsageClass.
     */
    public function testErrorWhenInvalidAnnotationIsUsed()
    {
        $reader = $this->getReader();
        $ref = new \ReflectionClass('Doctrine\Tests\Common\Annotations\Fixtures\InvalidAnnotationUsageClass');
        $reader->getClassAnnotations($ref);
    }

    public function testInvalidAnnotationUsageButIgnoredClass()
    {
        $reader = $this->getReader();
        $ref = new \ReflectionClass('Doctrine\Tests\Common\Annotations\Fixtures\InvalidAnnotationUsageButIgnoredClass');
        $annots = $reader->getClassAnnotations($ref);

        $this->assertEquals(2, count($annots));
    }

    /**
     * @group DDC-1660
     * @group regression
     */
    public function testInvalidAnnotationButIgnored()
    {
        $reader = $this->getReader();
        $class  = new \ReflectionClass('Doctrine\Tests\Common\Annotations\Fixtures\ClassDDC1660');

        $this->assertTrue(class_exists('Doctrine\Tests\Common\Annotations\Fixtures\Annotation\Version'));
        $this->assertCount(0, $reader->getClassAnnotations($class));
        $this->assertCount(0, $reader->getMethodAnnotations($class->getMethod('bar')));
        $this->assertCount(0, $reader->getPropertyAnnotations($class->getProperty('foo')));
    }

    abstract protected function getReader();
}

/**
 * @parseAnnotation("var")
 * @author Johannes M. Schmitt <schmittjoh@gmail.com>
 *
 */
class TestParseAnnotationClass
{
    /**
     * @var
     */
    private $field;
}

/**
 * @Name
 * @author Johannes M. Schmitt <schmittjoh@gmail.com>
 */
class TestIgnoresNonAnnotationsClass
{
}

class TestTopLevelAnnotationClass
{
    /**
     * @\TopLevelAnnotation
     */
    private $field;
}

class TestNonExistentAnnotationClass
{
    /**
     * @Foo\Bar\Name
     */
    private $field;
}

class TestAnnotationNotImportedClass
{
    /**
     * @NameFoo
     */
    private $field;
}

class TestChildClass
{
    /**
     * @\Doctrine\Tests\Common\Annotations\Foo\Name(name = "foo")
     */
    protected $child;
}

class TestParentClass extends TestChildClass
{
    /**
     * @\Doctrine\Tests\Common\Annotations\Bar\Name(name = "bar")
     */
    private $parent;
}

class TestImportWithConcreteAnnotation
{
    /**
     * @DummyAnnotation(dummyValue = "bar")
     */
    private $field;
}

/**
 * @ignoreAnnotation("var")
 */
class DummyClass2 {
    /**
     * @DummyId @DummyColumn(type="integer") @DummyGeneratedValue
     * @var integer
     */
    private $id;
}

/** @Annotation */
class DummyId extends \Doctrine\Common\Annotations\Annotation {}
/** @Annotation */
class DummyColumn extends \Doctrine\Common\Annotations\Annotation {
    public $type;
}
/** @Annotation */
class DummyGeneratedValue extends \Doctrine\Common\Annotations\Annotation {}
/** @Annotation */
class DummyAnnotation extends \Doctrine\Common\Annotations\Annotation {
    public $dummyValue;
}
/** @Annotation */
class DummyJoinColumn extends \Doctrine\Common\Annotations\Annotation {
    public $name;
    public $referencedColumnName;
}
/** @Annotation */
class DummyJoinTable extends \Doctrine\Common\Annotations\Annotation {
    public $name;
    public $joinColumns;
    public $inverseJoinColumns;
}

/**
 * @DummyAnnotation(@)
 */
class DummyClassSyntaxError
{

}

class DummyClassMethodSyntaxError
{
    /**
     * @DummyAnnotation(@)
     */
    public function foo()
    {

    }
}

class DummyClassPropertySyntaxError
{
    /**
     * @DummyAnnotation(@)
     */
    public $foo;
}

/**
 * @ignoreAnnotation({"since", "var"})
 */
class DummyClassNonAnnotationProblem
{
    /**
     * @DummyAnnotation
     *
     * @var \Test
     * @since 0.1
     */
    public $foo;
}


/**
* @DummyAnnotation Foo bar <foobar@1domain.com>
*/
class DummyClassWithEmail
{

}

namespace Doctrine\Tests\Common\Annotations\Foo;

/** @Annotation */
class Name extends \Doctrine\Common\Annotations\Annotation
{
    public $name;
}

namespace Doctrine\Tests\Common\Annotations\Bar;

/** @Annotation */
class Name extends \Doctrine\Common\Annotations\Annotation
{
    public $name;
}
