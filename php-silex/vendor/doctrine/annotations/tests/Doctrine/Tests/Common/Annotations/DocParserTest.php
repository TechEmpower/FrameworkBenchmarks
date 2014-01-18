<?php

namespace Doctrine\Tests\Common\Annotations;

use Doctrine\Common\Annotations\Annotation\IgnorePhpDoc;
use Doctrine\Common\Annotations\Annotation\IgnoreAnnotation;
use Doctrine\Common\Annotations\DocParser;
use Doctrine\Common\Annotations\AnnotationRegistry;
use Doctrine\Common\Annotations\Annotation\Target;
use Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithConstants;
use Doctrine\Tests\Common\Annotations\Fixtures\ClassWithConstants;
use Doctrine\Tests\Common\Annotations\Fixtures\IntefaceWithConstants;

class DocParserTest extends \PHPUnit_Framework_TestCase
{
    public function testNestedArraysWithNestedAnnotation()
    {
        $parser = $this->createTestParser();

        // Nested arrays with nested annotations
        $result = $parser->parse('@Name(foo={1,2, {"key"=@Name}})');
        $annot = $result[0];

        $this->assertTrue($annot instanceof Name);
        $this->assertNull($annot->value);
        $this->assertEquals(3, count($annot->foo));
        $this->assertEquals(1, $annot->foo[0]);
        $this->assertEquals(2, $annot->foo[1]);
        $this->assertTrue(is_array($annot->foo[2]));

        $nestedArray = $annot->foo[2];
        $this->assertTrue(isset($nestedArray['key']));
        $this->assertTrue($nestedArray['key'] instanceof Name);
    }

    public function testBasicAnnotations()
    {
        $parser = $this->createTestParser();

        // Marker annotation
        $result = $parser->parse("@Name");
        $annot = $result[0];
        $this->assertTrue($annot instanceof Name);
        $this->assertNull($annot->value);
        $this->assertNull($annot->foo);

        // Associative arrays
        $result = $parser->parse('@Name(foo={"key1" = "value1"})');
        $annot = $result[0];
        $this->assertNull($annot->value);
        $this->assertTrue(is_array($annot->foo));
        $this->assertTrue(isset($annot->foo['key1']));

        // Numerical arrays
        $result = $parser->parse('@Name({2="foo", 4="bar"})');
        $annot = $result[0];
        $this->assertTrue(is_array($annot->value));
        $this->assertEquals('foo', $annot->value[2]);
        $this->assertEquals('bar', $annot->value[4]);
        $this->assertFalse(isset($annot->value[0]));
        $this->assertFalse(isset($annot->value[1]));
        $this->assertFalse(isset($annot->value[3]));

        // Multiple values
        $result = $parser->parse('@Name(@Name, @Name)');
        $annot = $result[0];

        $this->assertTrue($annot instanceof Name);
        $this->assertTrue(is_array($annot->value));
        $this->assertTrue($annot->value[0] instanceof Name);
        $this->assertTrue($annot->value[1] instanceof Name);

        // Multiple types as values
        $result = $parser->parse('@Name(foo="Bar", @Name, {"key1"="value1", "key2"="value2"})');
        $annot = $result[0];

        $this->assertTrue($annot instanceof Name);
        $this->assertTrue(is_array($annot->value));
        $this->assertTrue($annot->value[0] instanceof Name);
        $this->assertTrue(is_array($annot->value[1]));
        $this->assertEquals('value1', $annot->value[1]['key1']);
        $this->assertEquals('value2', $annot->value[1]['key2']);

        // Complete docblock
        $docblock = <<<DOCBLOCK
/**
 * Some nifty class.
 *
 * @author Mr.X
 * @Name(foo="bar")
 */
DOCBLOCK;

        $result = $parser->parse($docblock);
        $this->assertEquals(1, count($result));
        $annot = $result[0];
        $this->assertTrue($annot instanceof Name);
        $this->assertEquals("bar", $annot->foo);
        $this->assertNull($annot->value);
   }

    public function testNamespacedAnnotations()
    {
        $parser = new DocParser;
        $parser->setIgnoreNotImportedAnnotations(true);

        $docblock = <<<DOCBLOCK
/**
 * Some nifty class.
 *
 * @package foo
 * @subpackage bar
 * @author Mr.X <mr@x.com>
 * @Doctrine\Tests\Common\Annotations\Name(foo="bar")
 * @ignore
 */
DOCBLOCK;

        $result = $parser->parse($docblock);
        $this->assertEquals(1, count($result));
        $annot = $result[0];
        $this->assertTrue($annot instanceof Name);
        $this->assertEquals("bar", $annot->foo);
    }

    /**
     * @group debug
     */
    public function testTypicalMethodDocBlock()
    {
        $parser = $this->createTestParser();

        $docblock = <<<DOCBLOCK
/**
 * Some nifty method.
 *
 * @since 2.0
 * @Doctrine\Tests\Common\Annotations\Name(foo="bar")
 * @param string \$foo This is foo.
 * @param mixed \$bar This is bar.
 * @return string Foo and bar.
 * @This is irrelevant
 * @Marker
 */
DOCBLOCK;

        $result = $parser->parse($docblock);
        $this->assertEquals(2, count($result));
        $this->assertTrue(isset($result[0]));
        $this->assertTrue(isset($result[1]));
        $annot = $result[0];
        $this->assertTrue($annot instanceof Name);
        $this->assertEquals("bar", $annot->foo);
        $marker = $result[1];
        $this->assertTrue($marker instanceof Marker);
    }


    public function testAnnotationWithoutConstructor()
    {
        $parser = $this->createTestParser();


        $docblock = <<<DOCBLOCK
/**
 * @SomeAnnotationClassNameWithoutConstructor("Some data")
 */
DOCBLOCK;

        $result     = $parser->parse($docblock);
        $this->assertEquals(count($result), 1);
        $annot      = $result[0];

        $this->assertNotNull($annot);
        $this->assertTrue($annot instanceof SomeAnnotationClassNameWithoutConstructor);

        $this->assertNull($annot->name);
        $this->assertNotNull($annot->data);
        $this->assertEquals($annot->data, "Some data");




$docblock = <<<DOCBLOCK
/**
 * @SomeAnnotationClassNameWithoutConstructor(name="Some Name", data = "Some data")
 */
DOCBLOCK;


        $result     = $parser->parse($docblock);
        $this->assertEquals(count($result), 1);
        $annot      = $result[0];

        $this->assertNotNull($annot);
        $this->assertTrue($annot instanceof SomeAnnotationClassNameWithoutConstructor);

        $this->assertEquals($annot->name, "Some Name");
        $this->assertEquals($annot->data, "Some data");




$docblock = <<<DOCBLOCK
/**
 * @SomeAnnotationClassNameWithoutConstructor(data = "Some data")
 */
DOCBLOCK;

        $result     = $parser->parse($docblock);
        $this->assertEquals(count($result), 1);
        $annot      = $result[0];

        $this->assertEquals($annot->data, "Some data");
        $this->assertNull($annot->name);


        $docblock = <<<DOCBLOCK
/**
 * @SomeAnnotationClassNameWithoutConstructor(name = "Some name")
 */
DOCBLOCK;

        $result     = $parser->parse($docblock);
        $this->assertEquals(count($result), 1);
        $annot      = $result[0];

        $this->assertEquals($annot->name, "Some name");
        $this->assertNull($annot->data);

        $docblock = <<<DOCBLOCK
/**
 * @SomeAnnotationClassNameWithoutConstructor("Some data")
 */
DOCBLOCK;

        $result     = $parser->parse($docblock);
        $this->assertEquals(count($result), 1);
        $annot      = $result[0];

        $this->assertEquals($annot->data, "Some data");
        $this->assertNull($annot->name);



        $docblock = <<<DOCBLOCK
/**
 * @SomeAnnotationClassNameWithoutConstructor("Some data",name = "Some name")
 */
DOCBLOCK;

        $result     = $parser->parse($docblock);
        $this->assertEquals(count($result), 1);
        $annot      = $result[0];

        $this->assertEquals($annot->name, "Some name");
        $this->assertEquals($annot->data, "Some data");


        $docblock = <<<DOCBLOCK
/**
 * @SomeAnnotationWithConstructorWithoutParams(name = "Some name")
 */
DOCBLOCK;

        $result     = $parser->parse($docblock);
        $this->assertEquals(count($result), 1);
        $annot      = $result[0];

        $this->assertEquals($annot->name, "Some name");
        $this->assertEquals($annot->data, "Some data");

        $docblock = <<<DOCBLOCK
/**
 * @SomeAnnotationClassNameWithoutConstructorAndProperties()
 */
DOCBLOCK;

        $result     = $parser->parse($docblock);
        $this->assertEquals(count($result), 1);
        $this->assertTrue($result[0] instanceof SomeAnnotationClassNameWithoutConstructorAndProperties);
    }

    public function testAnnotationTarget()
    {

        $parser = new DocParser;
        $parser->setImports(array(
            '__NAMESPACE__' => 'Doctrine\Tests\Common\Annotations\Fixtures',
        ));
        $class  = new \ReflectionClass('Doctrine\Tests\Common\Annotations\Fixtures\ClassWithValidAnnotationTarget');


        $context    = 'class ' . $class->getName();
        $docComment = $class->getDocComment();

        $parser->setTarget(Target::TARGET_CLASS);
        $this->assertNotNull($parser->parse($docComment,$context));


        $property   = $class->getProperty('foo');
        $docComment = $property->getDocComment();
        $context    = 'property ' . $class->getName() . "::\$" . $property->getName();

        $parser->setTarget(Target::TARGET_PROPERTY);
        $this->assertNotNull($parser->parse($docComment,$context));



        $method     = $class->getMethod('someFunction');
        $docComment = $property->getDocComment();
        $context    = 'method ' . $class->getName() . '::' . $method->getName() . '()';

        $parser->setTarget(Target::TARGET_METHOD);
        $this->assertNotNull($parser->parse($docComment,$context));


        try {
            $class      = new \ReflectionClass('Doctrine\Tests\Common\Annotations\Fixtures\ClassWithInvalidAnnotationTargetAtClass');
            $context    = 'class ' . $class->getName();
            $docComment = $class->getDocComment();

            $parser->setTarget(Target::TARGET_CLASS);
            $parser->parse($class->getDocComment(),$context);

            $this->fail();
        } catch (\Doctrine\Common\Annotations\AnnotationException $exc) {
            $this->assertNotNull($exc->getMessage());
        }


        try {

            $class      = new \ReflectionClass('Doctrine\Tests\Common\Annotations\Fixtures\ClassWithInvalidAnnotationTargetAtMethod');
            $method     = $class->getMethod('functionName');
            $docComment = $method->getDocComment();
            $context    = 'method ' . $class->getName() . '::' . $method->getName() . '()';

            $parser->setTarget(Target::TARGET_METHOD);
            $parser->parse($docComment,$context);

            $this->fail();
        } catch (\Doctrine\Common\Annotations\AnnotationException $exc) {
            $this->assertNotNull($exc->getMessage());
        }


        try {
            $class      = new \ReflectionClass('Doctrine\Tests\Common\Annotations\Fixtures\ClassWithInvalidAnnotationTargetAtProperty');
            $property   = $class->getProperty('foo');
            $docComment = $property->getDocComment();
            $context    = 'property ' . $class->getName() . "::\$" . $property->getName();

            $parser->setTarget(Target::TARGET_PROPERTY);
            $parser->parse($docComment,$context);

            $this->fail();
        } catch (\Doctrine\Common\Annotations\AnnotationException $exc) {
            $this->assertNotNull($exc->getMessage());
        }

    }

    public function getAnnotationVarTypeProviderValid()
    {
        //({attribute name}, {attribute value})
         return array(
            // mixed type
            array('mixed', '"String Value"'),
            array('mixed', 'true'),
            array('mixed', 'false'),
            array('mixed', '1'),
            array('mixed', '1.2'),
            array('mixed', '@Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll'),

            // boolean type
            array('boolean', 'true'),
            array('boolean', 'false'),

            // alias for internal type boolean
            array('bool', 'true'),
            array('bool', 'false'),

            // integer type
            array('integer', '0'),
            array('integer', '1'),
            array('integer', '123456789'),
            array('integer', '9223372036854775807'),

            // alias for internal type double
            array('float', '0.1'),
            array('float', '1.2'),
            array('float', '123.456'),

            // string type
            array('string', '"String Value"'),
            array('string', '"true"'),
            array('string', '"123"'),

              // array type
            array('array', '{@AnnotationExtendsAnnotationTargetAll}'),
            array('array', '{@AnnotationExtendsAnnotationTargetAll,@AnnotationExtendsAnnotationTargetAll}'),

            array('arrayOfIntegers', '1'),
            array('arrayOfIntegers', '{1}'),
            array('arrayOfIntegers', '{1,2,3,4}'),
            array('arrayOfAnnotations', '@AnnotationExtendsAnnotationTargetAll'),
            array('arrayOfAnnotations', '{@Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll}'),
            array('arrayOfAnnotations', '{@AnnotationExtendsAnnotationTargetAll, @Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll}'),

            // annotation instance
            array('annotation', '@Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll'),
            array('annotation', '@AnnotationExtendsAnnotationTargetAll'),
        );
    }

    public function getAnnotationVarTypeProviderInvalid()
    {
         //({attribute name}, {type declared type}, {attribute value} , {given type or class})
         return array(
            // boolean type
            array('boolean','boolean','1','integer'),
            array('boolean','boolean','1.2','double'),
            array('boolean','boolean','"str"','string'),
            array('boolean','boolean','{1,2,3}','array'),
            array('boolean','boolean','@Name', 'an instance of Doctrine\Tests\Common\Annotations\Name'),

            // alias for internal type boolean
            array('bool','bool', '1','integer'),
            array('bool','bool', '1.2','double'),
            array('bool','bool', '"str"','string'),
            array('bool','bool', '{"str"}','array'),

            // integer type
            array('integer','integer', 'true','boolean'),
            array('integer','integer', 'false','boolean'),
            array('integer','integer', '1.2','double'),
            array('integer','integer', '"str"','string'),
            array('integer','integer', '{"str"}','array'),
            array('integer','integer', '{1,2,3,4}','array'),

            // alias for internal type double
            array('float','float', 'true','boolean'),
            array('float','float', 'false','boolean'),
            array('float','float', '123','integer'),
            array('float','float', '"str"','string'),
            array('float','float', '{"str"}','array'),
            array('float','float', '{12.34}','array'),
            array('float','float', '{1,2,3}','array'),

            // string type
            array('string','string', 'true','boolean'),
            array('string','string', 'false','boolean'),
            array('string','string', '12','integer'),
            array('string','string', '1.2','double'),
            array('string','string', '{"str"}','array'),
            array('string','string', '{1,2,3,4}','array'),

             // annotation instance
            array('annotation','Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll', 'true','boolean'),
            array('annotation','Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll', 'false','boolean'),
            array('annotation','Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll', '12','integer'),
            array('annotation','Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll', '1.2','double'),
            array('annotation','Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll', '{"str"}','array'),
            array('annotation','Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll', '{1,2,3,4}','array'),
            array('annotation','Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll', '@Name','an instance of Doctrine\Tests\Common\Annotations\Name'),
        );
    }

    public function getAnnotationVarTypeArrayProviderInvalid()
    {
         //({attribute name}, {type declared type}, {attribute value} , {given type or class})
         return array(
            array('arrayOfIntegers','integer', 'true','boolean'),
            array('arrayOfIntegers','integer', 'false','boolean'),
            array('arrayOfIntegers','integer', '{true,true}','boolean'),
            array('arrayOfIntegers','integer', '{1,true}','boolean'),
            array('arrayOfIntegers','integer', '{1,2,1.2}','double'),
            array('arrayOfIntegers','integer', '{1,2,"str"}','string'),


            array('arrayOfAnnotations','Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll', 'true','boolean'),
            array('arrayOfAnnotations','Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll', 'false','boolean'),
            array('arrayOfAnnotations','Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll', '{@Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll,true}','boolean'),
            array('arrayOfAnnotations','Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll', '{@Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll,true}','boolean'),
            array('arrayOfAnnotations','Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll', '{@Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll,1.2}','double'),
            array('arrayOfAnnotations','Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll', '{@Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll,@AnnotationExtendsAnnotationTargetAll,"str"}','string'),
        );
    }

    /**
     * @dataProvider getAnnotationVarTypeProviderValid
     */
    public function testAnnotationWithVarType($attribute, $value)
    {
        $parser     = $this->createTestParser();
        $context    = 'property SomeClassName::$invalidProperty.';
        $docblock   = sprintf('@Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithVarType(%s = %s)',$attribute, $value);
        $parser->setTarget(Target::TARGET_PROPERTY);

        $result = $parser->parse($docblock, $context);

        $this->assertTrue(sizeof($result) === 1);
        $this->assertInstanceOf('Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithVarType', $result[0]);
        $this->assertNotNull($result[0]->$attribute);
    }

    /**
     * @dataProvider getAnnotationVarTypeProviderInvalid
     */
    public function testAnnotationWithVarTypeError($attribute,$type,$value,$given)
    {
        $parser     = $this->createTestParser();
        $context    = 'property SomeClassName::invalidProperty.';
        $docblock   = sprintf('@Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithVarType(%s = %s)',$attribute, $value);
        $parser->setTarget(Target::TARGET_PROPERTY);

        try {
            $parser->parse($docblock, $context);
            $this->fail();
        } catch (\Doctrine\Common\Annotations\AnnotationException $exc) {
            $this->assertContains("[Type Error] Attribute \"$attribute\" of @Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithVarType declared on property SomeClassName::invalidProperty. expects a(n) $type, but got $given.", $exc->getMessage());
        }
    }


    /**
     * @dataProvider getAnnotationVarTypeArrayProviderInvalid
     */
    public function testAnnotationWithVarTypeArrayError($attribute,$type,$value,$given)
    {
        $parser     = $this->createTestParser();
        $context    = 'property SomeClassName::invalidProperty.';
        $docblock   = sprintf('@Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithVarType(%s = %s)',$attribute, $value);
        $parser->setTarget(Target::TARGET_PROPERTY);

        try {
            $parser->parse($docblock, $context);
            $this->fail();
        } catch (\Doctrine\Common\Annotations\AnnotationException $exc) {
            $this->assertContains("[Type Error] Attribute \"$attribute\" of @Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithVarType declared on property SomeClassName::invalidProperty. expects either a(n) $type, or an array of {$type}s, but got $given.", $exc->getMessage());
        }
    }

    /**
     * @dataProvider getAnnotationVarTypeProviderValid
     */
    public function testAnnotationWithAttributes($attribute, $value)
    {
        $parser     = $this->createTestParser();
        $context    = 'property SomeClassName::$invalidProperty.';
        $docblock   = sprintf('@Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithAttributes(%s = %s)',$attribute, $value);
        $parser->setTarget(Target::TARGET_PROPERTY);

        $result = $parser->parse($docblock, $context);

        $this->assertTrue(sizeof($result) === 1);
        $this->assertInstanceOf('Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithAttributes', $result[0]);
        $getter = "get".ucfirst($attribute);
        $this->assertNotNull($result[0]->$getter());
    }

   /**
     * @dataProvider getAnnotationVarTypeProviderInvalid
     */
    public function testAnnotationWithAttributesError($attribute,$type,$value,$given)
    {
        $parser     = $this->createTestParser();
        $context    = 'property SomeClassName::invalidProperty.';
        $docblock   = sprintf('@Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithAttributes(%s = %s)',$attribute, $value);
        $parser->setTarget(Target::TARGET_PROPERTY);

        try {
            $parser->parse($docblock, $context);
            $this->fail();
        } catch (\Doctrine\Common\Annotations\AnnotationException $exc) {
            $this->assertContains("[Type Error] Attribute \"$attribute\" of @Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithAttributes declared on property SomeClassName::invalidProperty. expects a(n) $type, but got $given.", $exc->getMessage());
        }
    }


   /**
     * @dataProvider getAnnotationVarTypeArrayProviderInvalid
     */
    public function testAnnotationWithAttributesWithVarTypeArrayError($attribute,$type,$value,$given)
    {
        $parser     = $this->createTestParser();
        $context    = 'property SomeClassName::invalidProperty.';
        $docblock   = sprintf('@Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithAttributes(%s = %s)',$attribute, $value);
        $parser->setTarget(Target::TARGET_PROPERTY);

        try {
            $parser->parse($docblock, $context);
            $this->fail();
        } catch (\Doctrine\Common\Annotations\AnnotationException $exc) {
            $this->assertContains("[Type Error] Attribute \"$attribute\" of @Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithAttributes declared on property SomeClassName::invalidProperty. expects either a(n) $type, or an array of {$type}s, but got $given.", $exc->getMessage());
        }
    }

    public function testAnnotationWithRequiredAttributes()
    {
        $parser     = $this->createTestParser();
        $context    = 'property SomeClassName::invalidProperty.';
        $parser->setTarget(Target::TARGET_PROPERTY);


        $docblock   = '@Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithRequiredAttributes("Some Value", annot = @Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAnnotation)';
        $result     = $parser->parse($docblock);

        $this->assertTrue(sizeof($result) === 1);
        $this->assertInstanceOf('Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithRequiredAttributes', $result[0]);
        $this->assertEquals("Some Value",$result[0]->getValue());
        $this->assertInstanceOf('Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAnnotation', $result[0]->getAnnot());


        $docblock   = '@Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithRequiredAttributes("Some Value")';
        try {
            $result = $parser->parse($docblock,$context);
            $this->fail();
        } catch (\Doctrine\Common\Annotations\AnnotationException $exc) {
            $this->assertContains('Attribute "annot" of @Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithRequiredAttributes declared on property SomeClassName::invalidProperty. expects a(n) Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAnnotation. This value should not be null.', $exc->getMessage());
        }

        $docblock   = '@Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithRequiredAttributes(annot = @Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAnnotation)';
        try {
            $result = $parser->parse($docblock,$context);
            $this->fail();
        } catch (\Doctrine\Common\Annotations\AnnotationException $exc) {
            $this->assertContains('Attribute "value" of @Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithRequiredAttributes declared on property SomeClassName::invalidProperty. expects a(n) string. This value should not be null.', $exc->getMessage());
        }

    }

    public function testAnnotationWithRequiredAttributesWithoutContructor()
    {
        $parser     = $this->createTestParser();
        $context    = 'property SomeClassName::invalidProperty.';
        $parser->setTarget(Target::TARGET_PROPERTY);


        $docblock   = '@Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithRequiredAttributesWithoutContructor("Some Value", annot = @Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAnnotation)';
        $result     = $parser->parse($docblock);

        $this->assertTrue(sizeof($result) === 1);
        $this->assertInstanceOf('Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithRequiredAttributesWithoutContructor', $result[0]);
        $this->assertEquals("Some Value", $result[0]->value);
        $this->assertInstanceOf('Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAnnotation', $result[0]->annot);


        $docblock   = '@Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithRequiredAttributesWithoutContructor("Some Value")';
        try {
            $result = $parser->parse($docblock,$context);
            $this->fail();
        } catch (\Doctrine\Common\Annotations\AnnotationException $exc) {
            $this->assertContains('Attribute "annot" of @Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithRequiredAttributesWithoutContructor declared on property SomeClassName::invalidProperty. expects a(n) Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAnnotation. This value should not be null.', $exc->getMessage());
        }

        $docblock   = '@Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithRequiredAttributesWithoutContructor(annot = @Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAnnotation)';
        try {
            $result = $parser->parse($docblock,$context);
            $this->fail();
        } catch (\Doctrine\Common\Annotations\AnnotationException $exc) {
            $this->assertContains('Attribute "value" of @Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithRequiredAttributesWithoutContructor declared on property SomeClassName::invalidProperty. expects a(n) string. This value should not be null.', $exc->getMessage());
        }

    }

    /**
     * @expectedException Doctrine\Common\Annotations\AnnotationException
     * @expectedExceptionMessage Attribute "value" of @Doctrine\Tests\Common\Annotations\Fixtures\AnnotationEnum declared on property SomeClassName::invalidProperty. accept only [ONE, TWO, THREE], but got FOUR.
     */
    public function testAnnotationEnumeratorException()
    {
        $parser     = $this->createTestParser();
        $context    = 'property SomeClassName::invalidProperty.';
        $docblock   = '@Doctrine\Tests\Common\Annotations\Fixtures\AnnotationEnum("FOUR")';

        $parser->setIgnoreNotImportedAnnotations(false);
        $parser->setTarget(Target::TARGET_PROPERTY);
        $parser->parse($docblock, $context);
    }

    /**
     * @expectedException Doctrine\Common\Annotations\AnnotationException
     * @expectedExceptionMessage Attribute "value" of @Doctrine\Tests\Common\Annotations\Fixtures\AnnotationEnumLiteral declared on property SomeClassName::invalidProperty. accept only [AnnotationEnumLiteral::ONE, AnnotationEnumLiteral::TWO, AnnotationEnumLiteral::THREE], but got 4.
     */
    public function testAnnotationEnumeratorLiteralException()
    {
        $parser     = $this->createTestParser();
        $context    = 'property SomeClassName::invalidProperty.';
        $docblock   = '@Doctrine\Tests\Common\Annotations\Fixtures\AnnotationEnumLiteral(4)';

        $parser->setIgnoreNotImportedAnnotations(false);
        $parser->setTarget(Target::TARGET_PROPERTY);
        $parser->parse($docblock, $context);
    }
   
    /**
     * @expectedException \InvalidArgumentException
     * @expectedExceptionMessage @Enum supports only scalar values "array" given.
     */
    public function testAnnotationEnumInvalidTypeDeclarationException()
    {
        $parser     = $this->createTestParser();
        $docblock   = '@Doctrine\Tests\Common\Annotations\Fixtures\AnnotationEnumInvalid("foo")';

        $parser->setIgnoreNotImportedAnnotations(false);
        $parser->parse($docblock);
    }

    /**
     * @expectedException \InvalidArgumentException
     * @expectedExceptionMessage Undefined enumerator value "3" for literal "AnnotationEnumLiteral::THREE".
     */
    public function testAnnotationEnumInvalidLiteralDeclarationException()
    {
        $parser     = $this->createTestParser();
        $docblock   = '@Doctrine\Tests\Common\Annotations\Fixtures\AnnotationEnumLiteralInvalid("foo")';

        $parser->setIgnoreNotImportedAnnotations(false);
        $parser->parse($docblock);
    }
    
    public function getConstantsProvider()
    {
        $provider[] = array(
            '@AnnotationWithConstants(PHP_EOL)',
            PHP_EOL
        );
        $provider[] = array(
            '@AnnotationWithConstants(AnnotationWithConstants::INTEGER)',
            AnnotationWithConstants::INTEGER
        );
        $provider[] = array(
            '@Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithConstants(AnnotationWithConstants::STRING)',
            AnnotationWithConstants::STRING
        );
        $provider[] = array(
            '@AnnotationWithConstants(Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithConstants::FLOAT)',
            AnnotationWithConstants::FLOAT
        );
        $provider[] = array(
            '@AnnotationWithConstants(ClassWithConstants::SOME_VALUE)',
            ClassWithConstants::SOME_VALUE
        );
        $provider[] = array(
            '@AnnotationWithConstants(Doctrine\Tests\Common\Annotations\Fixtures\ClassWithConstants::SOME_VALUE)',
            ClassWithConstants::SOME_VALUE
        );
        $provider[] = array(
            '@AnnotationWithConstants(IntefaceWithConstants::SOME_VALUE)',
            IntefaceWithConstants::SOME_VALUE
        );
        $provider[] = array(
            '@AnnotationWithConstants(\Doctrine\Tests\Common\Annotations\Fixtures\IntefaceWithConstants::SOME_VALUE)',
            IntefaceWithConstants::SOME_VALUE
        );
        $provider[] = array(
            '@AnnotationWithConstants({AnnotationWithConstants::STRING, AnnotationWithConstants::INTEGER, AnnotationWithConstants::FLOAT})',
            array(AnnotationWithConstants::STRING, AnnotationWithConstants::INTEGER, AnnotationWithConstants::FLOAT)
        );
        $provider[] = array(
            '@AnnotationWithConstants({
                AnnotationWithConstants::STRING = AnnotationWithConstants::INTEGER
             })',
            array(AnnotationWithConstants::STRING => AnnotationWithConstants::INTEGER)
        );
        $provider[] = array(
            '@AnnotationWithConstants({
                Doctrine\Tests\Common\Annotations\Fixtures\IntefaceWithConstants::SOME_KEY = AnnotationWithConstants::INTEGER
             })',
            array(IntefaceWithConstants::SOME_KEY => AnnotationWithConstants::INTEGER)
        );
        $provider[] = array(
            '@AnnotationWithConstants({
                \Doctrine\Tests\Common\Annotations\Fixtures\IntefaceWithConstants::SOME_KEY = AnnotationWithConstants::INTEGER
             })',
            array(IntefaceWithConstants::SOME_KEY => AnnotationWithConstants::INTEGER)
        );
        $provider[] = array(
            '@AnnotationWithConstants({
                AnnotationWithConstants::STRING = AnnotationWithConstants::INTEGER,
                ClassWithConstants::SOME_KEY = ClassWithConstants::SOME_VALUE,
                Doctrine\Tests\Common\Annotations\Fixtures\ClassWithConstants::SOME_KEY = IntefaceWithConstants::SOME_VALUE
             })',
            array(
                AnnotationWithConstants::STRING => AnnotationWithConstants::INTEGER,
                ClassWithConstants::SOME_KEY    => ClassWithConstants::SOME_VALUE,
                ClassWithConstants::SOME_KEY    => IntefaceWithConstants::SOME_VALUE
            )
        );
        return $provider;
    }

    /**
     * @dataProvider getConstantsProvider
     */
    public function testSupportClassConstants($docblock, $expected)
    {
        $parser = $this->createTestParser();
        $parser->setImports(array(
            'classwithconstants'        => 'Doctrine\Tests\Common\Annotations\Fixtures\ClassWithConstants',
            'intefacewithconstants'     => 'Doctrine\Tests\Common\Annotations\Fixtures\IntefaceWithConstants',
            'annotationwithconstants'   => 'Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithConstants'
        ));
        
        $result = $parser->parse($docblock);
        $this->assertInstanceOf('\Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithConstants', $annotation = $result[0]);
        $this->assertEquals($expected, $annotation->value);
    }

    /**
     * @expectedException Doctrine\Common\Annotations\AnnotationException
     * @expectedExceptionMessage The annotation @SomeAnnotationClassNameWithoutConstructorAndProperties declared on  does not accept any values, but got {"value":"Foo"}.
     */
    public function testWithoutConstructorWhenIsNotDefaultValue()
    {
        $parser     = $this->createTestParser();
        $docblock   = <<<DOCBLOCK
/**
 * @SomeAnnotationClassNameWithoutConstructorAndProperties("Foo")
 */
DOCBLOCK;


        $parser->setTarget(Target::TARGET_CLASS);
        $parser->parse($docblock);
    }

    /**
     * @expectedException Doctrine\Common\Annotations\AnnotationException
     * @expectedExceptionMessage The annotation @SomeAnnotationClassNameWithoutConstructorAndProperties declared on  does not accept any values, but got {"value":"Foo"}.
     */
    public function testWithoutConstructorWhenHasNoProperties()
    {
        $parser     = $this->createTestParser();
        $docblock   = <<<DOCBLOCK
/**
 * @SomeAnnotationClassNameWithoutConstructorAndProperties(value = "Foo")
 */
DOCBLOCK;

        $parser->setTarget(Target::TARGET_CLASS);
        $parser->parse($docblock);
    }

    /**
     * @expectedException Doctrine\Common\Annotations\AnnotationException
     * @expectedExceptionMessage Expected namespace separator or identifier, got ')' at position 24 in class @Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithTargetSyntaxError.
     */
    public function testAnnotationTargetSyntaxError()
    {
        $parser     = $this->createTestParser();
        $context    = 'class ' . 'SomeClassName';
        $docblock   = <<<DOCBLOCK
/**
 * @Doctrine\Tests\Common\Annotations\Fixtures\AnnotationWithTargetSyntaxError()
 */
DOCBLOCK;

        $parser->setTarget(Target::TARGET_CLASS);
        $parser->parse($docblock,$context);
    }

    /**
     * @expectedException \InvalidArgumentException
     * @expectedExceptionMessage Invalid Target "Foo". Available targets: [ALL, CLASS, METHOD, PROPERTY, ANNOTATION]
     */
    public function testAnnotationWithInvalidTargetDeclarationError()
    {
        $parser     = $this->createTestParser();
        $context    = 'class ' . 'SomeClassName';
        $docblock   = <<<DOCBLOCK
/**
 * @AnnotationWithInvalidTargetDeclaration()
 */
DOCBLOCK;

        $parser->setTarget(Target::TARGET_CLASS);
        $parser->parse($docblock,$context);
    }

    /**
     * @expectedException \InvalidArgumentException
     * @expectedExceptionMessage @Target expects either a string value, or an array of strings, "NULL" given.
     */
    public function testAnnotationWithTargetEmptyError()
    {
        $parser     = $this->createTestParser();
        $context    = 'class ' . 'SomeClassName';
        $docblock   = <<<DOCBLOCK
/**
 * @AnnotationWithTargetEmpty()
 */
DOCBLOCK;

        $parser->setTarget(Target::TARGET_CLASS);
        $parser->parse($docblock,$context);
    }

    /**
     * @group DDC-575
     */
    public function testRegressionDDC575()
    {
        $parser = $this->createTestParser();

        $docblock = <<<DOCBLOCK
/**
 * @Name
 *
 * Will trigger error.
 */
DOCBLOCK;

        $result = $parser->parse($docblock);

        $this->assertInstanceOf("Doctrine\Tests\Common\Annotations\Name", $result[0]);

        $docblock = <<<DOCBLOCK
/**
 * @Name
 * @Marker
 *
 * Will trigger error.
 */
DOCBLOCK;

        $result = $parser->parse($docblock);

        $this->assertInstanceOf("Doctrine\Tests\Common\Annotations\Name", $result[0]);
    }

    /**
     * @group DDC-77
     */
    public function testAnnotationWithoutClassIsIgnoredWithoutWarning()
    {
        $parser = new DocParser();
        $parser->setIgnoreNotImportedAnnotations(true);
        $result = $parser->parse("@param");

        $this->assertEquals(0, count($result));
    }

    /**
     * @expectedException Doctrine\Common\Annotations\AnnotationException
     * @expectedExceptionMessage Expected PlainValue, got ''' at position 10.
     */
    public function testAnnotationDontAcceptSingleQuotes()
    {
        $parser = $this->createTestParser();
        $parser->parse("@Name(foo='bar')");
    }

    /**
     * @group DCOM-41
     */
    public function testAnnotationDoesntThrowExceptionWhenAtSignIsNotFollowedByIdentifier()
    {
        $parser = new DocParser();
        $result = $parser->parse("'@'");

        $this->assertEquals(0, count($result));
    }

    /**
     * @group DCOM-41
     * @expectedException Doctrine\Common\Annotations\AnnotationException
     */
    public function testAnnotationThrowsExceptionWhenAtSignIsNotFollowedByIdentifierInNestedAnnotation()
    {
        $parser = new DocParser();
        $result = $parser->parse("@Doctrine\Tests\Common\Annotations\Name(@')");
    }

    /**
     * @group DCOM-56
     */
    public function testAutoloadAnnotation()
    {
        $this->assertFalse(class_exists('Doctrine\Tests\Common\Annotations\Fixture\Annotation\Autoload', false), 'Pre-condition: Doctrine\Tests\Common\Annotations\Fixture\Annotation\Autoload not allowed to be loaded.');

        $parser = new DocParser();

        AnnotationRegistry::registerAutoloadNamespace('Doctrine\Tests\Common\Annotations\Fixtures\Annotation', __DIR__ . '/../../../../');

        $parser->setImports(array(
            'autoload' => 'Doctrine\Tests\Common\Annotations\Fixtures\Annotation\Autoload',
        ));
        $annotations = $parser->parse('@Autoload');

        $this->assertEquals(1, count($annotations));
        $this->assertInstanceOf('Doctrine\Tests\Common\Annotations\Fixtures\Annotation\Autoload', $annotations[0]);
    }

    public function createTestParser()
    {
        $parser = new DocParser();
        $parser->setIgnoreNotImportedAnnotations(true);
        $parser->setImports(array(
            'name' => 'Doctrine\Tests\Common\Annotations\Name',
            '__NAMESPACE__' => 'Doctrine\Tests\Common\Annotations',
        ));

        return $parser;
    }

    /**
     * @group DDC-78
     * @expectedException Doctrine\Common\Annotations\AnnotationException
     * @expectedExceptionMessage Expected PlainValue, got ''' at position 10 in class \Doctrine\Tests\Common\Annotations\Name
     */
    public function testSyntaxErrorWithContextDescription()
    {
        $parser = $this->createTestParser();
        $parser->parse("@Name(foo='bar')", "class \Doctrine\Tests\Common\Annotations\Name");
    }

    /**
     * @group DDC-183
     */
    public function testSyntaxErrorWithUnknownCharacters()
    {
        $docblock = <<<DOCBLOCK
/**
 * @test at.
 */
class A {
}
DOCBLOCK;

        //$lexer = new \Doctrine\Common\Annotations\Lexer();
        //$lexer->setInput(trim($docblock, '/ *'));
        //var_dump($lexer);

        try {
            $parser = $this->createTestParser();
            $result = $parser->parse($docblock);
        } catch (Exception $e) {
            $this->fail($e->getMessage());
        }
    }

    /**
     * @group DCOM-14
     */
    public function testIgnorePHPDocThrowTag()
    {
        $docblock = <<<DOCBLOCK
/**
 * @throws \RuntimeException
 */
class A {
}
DOCBLOCK;

        try {
            $parser = $this->createTestParser();
            $result = $parser->parse($docblock);
        } catch (Exception $e) {
            $this->fail($e->getMessage());
        }
    }

    /**
     * @group DCOM-38
     */
    public function testCastInt()
    {
        $parser = $this->createTestParser();

        $result = $parser->parse("@Name(foo=1234)");
        $annot = $result[0];
        $this->assertInternalType('int', $annot->foo);
    }

    /**
     * @group DCOM-38
     */
    public function testCastNegativeInt()
    {
        $parser = $this->createTestParser();

        $result = $parser->parse("@Name(foo=-1234)");
        $annot = $result[0];
        $this->assertInternalType('int', $annot->foo);
    }

    /**
     * @group DCOM-38
     */
    public function testCastFloat()
    {
        $parser = $this->createTestParser();

        $result = $parser->parse("@Name(foo=1234.345)");
        $annot = $result[0];
        $this->assertInternalType('float', $annot->foo);
    }

    /**
     * @group DCOM-38
     */
    public function testCastNegativeFloat()
    {
        $parser = $this->createTestParser();

        $result = $parser->parse("@Name(foo=-1234.345)");
        $annot = $result[0];
        $this->assertInternalType('float', $annot->foo);

        $result = $parser->parse("@Marker(-1234.345)");
        $annot = $result[0];
        $this->assertInternalType('float', $annot->value);
    }

    public function testReservedKeywordsInAnnotations()
    {
        $parser = $this->createTestParser();

        $result = $parser->parse('@Doctrine\Tests\Common\Annotations\True');
        $this->assertTrue($result[0] instanceof True);
        $result = $parser->parse('@Doctrine\Tests\Common\Annotations\False');
        $this->assertTrue($result[0] instanceof False);
        $result = $parser->parse('@Doctrine\Tests\Common\Annotations\Null');
        $this->assertTrue($result[0] instanceof Null);

        $result = $parser->parse('@True');
        $this->assertTrue($result[0] instanceof True);
        $result = $parser->parse('@False');
        $this->assertTrue($result[0] instanceof False);
        $result = $parser->parse('@Null');
        $this->assertTrue($result[0] instanceof Null);
    }

     /**
     * @expectedException Doctrine\Common\Annotations\AnnotationException
     * @expectedExceptionMessage [Creation Error] The annotation @SomeAnnotationClassNameWithoutConstructor declared on some class does not have a property named "invalidaProperty". Available properties: data, name
     */
    public function testSetValuesExeption()
    {
        $docblock = <<<DOCBLOCK
/**
 * @SomeAnnotationClassNameWithoutConstructor(invalidaProperty = "Some val")
 */
DOCBLOCK;

        $this->createTestParser()->parse($docblock, 'some class');
    }

    /**
     * @expectedException Doctrine\Common\Annotations\AnnotationException
     * @expectedExceptionMessage [Syntax Error] Expected Doctrine\Common\Annotations\DocLexer::T_IDENTIFIER or Doctrine\Common\Annotations\DocLexer::T_TRUE or Doctrine\Common\Annotations\DocLexer::T_FALSE or Doctrine\Common\Annotations\DocLexer::T_NULL, got '3.42' at position 5.
     */
    public function testInvalidIdentifierInAnnotation()
    {
        $parser = $this->createTestParser();
        $parser->parse('@Foo\3.42');
    }

    public function testTrailingCommaIsAllowed()
    {
        $parser = $this->createTestParser();

        $annots = $parser->parse('@Name({
            "Foo",
            "Bar",
        })');
        $this->assertEquals(1, count($annots));
        $this->assertEquals(array('Foo', 'Bar'), $annots[0]->value);
    }

    public function testDefaultAnnotationValueIsNotOverwritten()
    {
        $parser = $this->createTestParser();

        $annots = $parser->parse('@Doctrine\Tests\Common\Annotations\Fixtures\Annotation\AnnotWithDefaultValue');
        $this->assertEquals(1, count($annots));
        $this->assertEquals('bar', $annots[0]->foo);
    }

    public function testArrayWithColon()
    {
        $parser = $this->createTestParser();

        $annots = $parser->parse('@Name({"foo": "bar"})');
        $this->assertEquals(1, count($annots));
        $this->assertEquals(array('foo' => 'bar'), $annots[0]->value);
    }

    /**
     * @expectedException Doctrine\Common\Annotations\AnnotationException
     * @expectedExceptionMessage [Semantical Error] Couldn't find constant foo.
     */
    public function testInvalidContantName()
    {
        $parser = $this->createTestParser();
        $parser->parse('@Name(foo: "bar")');
    }

    /**
     * Tests parsing empty arrays.
     */
    public function testEmptyArray()
    {
        $parser = $this->createTestParser();

        $annots = $parser->parse('@Name({"foo": {}})');
        $this->assertEquals(1, count($annots));
        $this->assertEquals(array('foo' => array()), $annots[0]->value);
    }
}

/** @Annotation */
class SomeAnnotationClassNameWithoutConstructor
{
    public $data;
    public $name;
}

/** @Annotation */
class SomeAnnotationWithConstructorWithoutParams
{
    function __construct()
    {
        $this->data = "Some data";
    }
    public $data;
    public $name;
}

/** @Annotation */
class SomeAnnotationClassNameWithoutConstructorAndProperties{}

/**
 * @Annotation
 * @Target("Foo")
 */
class AnnotationWithInvalidTargetDeclaration{}

/**
 * @Annotation
 * @Target
 */
class AnnotationWithTargetEmpty{}

/** @Annotation */
class AnnotationExtendsAnnotationTargetAll extends \Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll
{
}

/** @Annotation */
class Name extends \Doctrine\Common\Annotations\Annotation {
    public $foo;
}

/** @Annotation */
class Marker {
    public $value;
}

/** @Annotation */
class True {}

/** @Annotation */
class False {}

/** @Annotation */
class Null {}

namespace Doctrine\Tests\Common\Annotations\FooBar;

/** @Annotation */
class Name extends \Doctrine\Common\Annotations\Annotation {
}
