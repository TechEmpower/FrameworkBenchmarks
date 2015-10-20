<?php
/*
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * This software consists of voluntary contributions made by many individuals
 * and is licensed under the MIT license. For more information, see
 * <http://www.doctrine-project.org>.
 */


namespace Doctrine\Tests\Common\Proxy;

use Doctrine\Common\Proxy\ProxyGenerator;
use ReflectionClass;
use ReflectionMethod;
use PHPUnit_Framework_TestCase;

/**
 * Test the proxy generator. Its work is generating on-the-fly subclasses of a given model, which implement the Proxy
 * pattern.
 *
 * @author Giorgio Sironi <piccoloprincipeazzurro@gmail.com>
 * @author Marco Pivetta <ocramius@gmail.com>
 */
class ProxyClassGeneratorTest extends PHPUnit_Framework_TestCase
{
    /**
     * @var string
     */
    protected $proxyClass = 'Doctrine\Tests\Common\ProxyProxy\__CG__\Doctrine\Tests\Common\Proxy\LazyLoadableObject';

    /**
     * @var LazyLoadableObjectClassMetadata
     */
    protected $metadata;

    /**
     * @var ProxyGenerator
     */
    protected $proxyGenerator;

    /**
     * {@inheritDoc}
     */
    protected function setUp()
    {
        $this->metadata       = new LazyLoadableObjectClassMetadata();
        $this->proxyGenerator = new ProxyGenerator(__DIR__ . '/generated', __NAMESPACE__ . 'Proxy', true);

        if (class_exists($this->proxyClass, false)) {
            return;
        }

        $this->generateAndRequire($this->proxyGenerator, $this->metadata);
    }

    public function testReferenceProxyRespectsMethodsParametersTypeHinting()
    {
        $method = new ReflectionMethod($this->proxyClass, 'publicTypeHintedMethod');
        $params = $method->getParameters();

        $this->assertEquals(1, count($params));
        $this->assertEquals('stdClass', $params[0]->getClass()->getName());
    }

    public function testProxyRespectsMethodsWhichReturnValuesByReference()
    {
        $method = new ReflectionMethod($this->proxyClass, 'byRefMethod');

        $this->assertTrue($method->returnsReference());
    }

    public function testProxyRespectsByRefMethodParameters()
    {
        $method = new ReflectionMethod($this->proxyClass, 'byRefParamMethod');
        $parameters = $method->getParameters();
        $this->assertSame('thisIsNotByRef', $parameters[0]->getName());
        $this->assertFalse($parameters[0]->isPassedByReference());
        $this->assertSame('thisIsByRef', $parameters[1]->getName());
        $this->assertTrue($parameters[1]->isPassedByReference());
    }

    public function testCreatesAssociationProxyAsSubclassOfTheOriginalOne()
    {
        $this->assertTrue(is_subclass_of($this->proxyClass, $this->metadata->getName()));
    }

    public function testNonNamespacedProxyGeneration()
    {
        $classCode = file_get_contents($this->proxyGenerator->getProxyFileName($this->metadata->getName()));

        $this->assertNotContains("class LazyLoadableObject extends \\\\" . $this->metadata->getName(), $classCode);
        $this->assertContains("class LazyLoadableObject extends \\" . $this->metadata->getName(), $classCode);
    }

    public function testClassWithSleepProxyGeneration()
    {
        if (!class_exists('Doctrine\Tests\Common\ProxyProxy\__CG__\SleepClass', false)) {
            $className = 'Doctrine\Tests\Common\Proxy\SleepClass';
            $metadata = $this->createClassMetadata($className, array('id'));
            $proxyGenerator = new ProxyGenerator(__DIR__ . '/generated', __NAMESPACE__ . 'Proxy', true);

            $this->generateAndRequire($proxyGenerator, $metadata);
        }

        $classCode = file_get_contents(__DIR__ . '/generated/__CG__DoctrineTestsCommonProxySleepClass.php');
        $this->assertEquals(1, substr_count($classCode, 'function __sleep'));
        $this->assertEquals(1, substr_count($classCode, 'parent::__sleep()'));
    }

    private function generateAndRequire($proxyGenerator, $metadata)
    {
        $proxyGenerator->generateProxyClass($metadata, $proxyGenerator->getProxyFileName($metadata->getName()));

        require_once $proxyGenerator->getProxyFileName($metadata->getName());
    }

    public function testClassWithCallableTypeHintOnProxiedMethod()
    {
        if (PHP_VERSION_ID < 50400) {
            $this->markTestSkipped('`callable` is only supported in PHP >=5.4.0');
        }

        if (!class_exists('Doctrine\Tests\Common\ProxyProxy\__CG__\CallableTypeHintClass', false)) {
            $className = 'Doctrine\Tests\Common\Proxy\CallableTypeHintClass';
            $metadata = $this->createClassMetadata($className, array('id'));

            $proxyGenerator = new ProxyGenerator(__DIR__ . '/generated', __NAMESPACE__ . 'Proxy', true);
            $this->generateAndRequire($proxyGenerator, $metadata);
        }

        $classCode = file_get_contents(__DIR__ . '/generated/__CG__DoctrineTestsCommonProxyCallableTypeHintClass.php');

        $this->assertEquals(1, substr_count($classCode, 'call(callable $foo)'));
    }

    public function testClassWithInvalidTypeHintOnProxiedMethod()
    {
        $className = 'Doctrine\Tests\Common\Proxy\InvalidTypeHintClass';
        $metadata = $this->createClassMetadata($className, array('id'));
        $proxyGenerator = new ProxyGenerator(__DIR__ . '/generated', __NAMESPACE__ . 'Proxy', true);

        $this->setExpectedException(
            'Doctrine\Common\Proxy\Exception\UnexpectedValueException',
            'The type hint of parameter "foo" in method "invalidTypeHintMethod"'
                .' in class "' . $className . '" is invalid.'
        );
        $proxyGenerator->generateProxyClass($metadata);
    }

    public function testNoConfigDirThrowsException()
    {
        $this->setExpectedException('Doctrine\Common\Proxy\Exception\InvalidArgumentException');
        new ProxyGenerator(null, null);
    }

    public function testNoNamespaceThrowsException()
    {
        $this->setExpectedException('Doctrine\Common\Proxy\Exception\InvalidArgumentException');
        new ProxyGenerator(__DIR__ . '/generated', null);
    }

    public function testInvalidPlaceholderThrowsException()
    {
        $this->setExpectedException('Doctrine\Common\Proxy\Exception\InvalidArgumentException');
        $generator = new ProxyGenerator(__DIR__ . '/generated', 'SomeNamespace');
        $generator->setPlaceholder('<somePlaceholder>', array());
    }

    public function testUseEvalIfNoFilenameIsGiven()
    {
        $proxyGenerator = new ProxyGenerator(__DIR__ . '/generated', __NAMESPACE__ . 'Proxy', true);

        $className = __NAMESPACE__ . '\\EvalBase';

        $metadata = $this->createClassMetadata($className, array('id'));

        $proxyGenerator->generateProxyClass($metadata);

        $reflClass = new ReflectionClass('Doctrine\Tests\Common\ProxyProxy\__CG__\Doctrine\Tests\Common\Proxy\EvalBase');

        $this->assertContains("eval()'d code", $reflClass->getFileName());
    }

    private function createClassMetadata($className, array $ids)
    {
        $metadata = $this->getMock('Doctrine\Common\Persistence\Mapping\ClassMetadata');
        $reflClass = new ReflectionClass($className);
        $metadata->expects($this->any())->method('getReflectionClass')->will($this->returnValue($reflClass));
        $metadata->expects($this->any())->method('getIdentifierFieldNames')->will($this->returnValue($ids));
        $metadata->expects($this->any())->method('getName')->will($this->returnValue($className));

        return $metadata;
    }
}

class EvalBase
{
}
