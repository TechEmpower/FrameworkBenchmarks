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
use Doctrine\Common\Proxy\Proxy;
use Doctrine\Common\Proxy\Exception\UnexpectedValueException;
use Doctrine\Common\Persistence\Mapping\ClassMetadata;
use PHPUnit_Framework_TestCase;

/**
 * Test the generated proxies behavior. These tests make assumptions about the structure of LazyLoadableObject
 *
 * @author Marco Pivetta <ocramius@gmail.com>
 */
class ProxyLogicTest extends PHPUnit_Framework_TestCase
{
    /**
     * @var \PHPUnit_Framework_MockObject_MockObject
     */
    protected $proxyLoader;

    /**
     * @var ClassMetadata
     */
    protected $lazyLoadableObjectMetadata;

    /**
     * @var LazyLoadableObject|Proxy
     */
    protected $lazyObject;

    protected $identifier = array(
        'publicIdentifierField' => 'publicIdentifierFieldValue',
        'protectedIdentifierField' => 'protectedIdentifierFieldValue',
    );

    /**
     * @var \PHPUnit_Framework_MockObject_MockObject|Callable
     */
    protected $initializerCallbackMock;

    /**
     * {@inheritDoc}
     */
    public function setUp()
    {
        $this->proxyLoader = $loader      = $this->getMock('stdClass', array('load'), array(), '', false);
        $this->initializerCallbackMock    = $this->getMock('stdClass', array('__invoke'));
        $identifier                       = $this->identifier;
        $this->lazyLoadableObjectMetadata = $metadata = new LazyLoadableObjectClassMetadata();

        // emulating what should happen in a proxy factory
        $cloner = function (LazyLoadableObject $proxy) use ($loader, $identifier, $metadata) {
            /* @var $proxy LazyLoadableObject|Proxy */
            if ($proxy->__isInitialized()) {
                return;
            }

            $proxy->__setInitialized(true);
            $proxy->__setInitializer(null);
            $original = $loader->load($identifier);

            if (null === $original) {
                throw new UnexpectedValueException();
            }

            foreach ($metadata->getReflectionClass()->getProperties() as $reflProperty) {
                $propertyName = $reflProperty->getName();

                if ($metadata->hasField($propertyName) || $metadata->hasAssociation($propertyName)) {
                    $reflProperty->setAccessible(true);
                    $reflProperty->setValue($proxy, $reflProperty->getValue($original));
                }
            }
        };

        $proxyClassName = 'Doctrine\Tests\Common\ProxyProxy\__CG__\Doctrine\Tests\Common\Proxy\LazyLoadableObject';

        // creating the proxy class
        if (!class_exists($proxyClassName, false)) {
            $proxyGenerator = new ProxyGenerator(__DIR__ . '/generated', __NAMESPACE__ . 'Proxy', true);
            $proxyGenerator->generateProxyClass($metadata);
            require_once $proxyGenerator->getProxyFileName($metadata->getName());
        }

        $this->lazyObject = new $proxyClassName($this->getClosure($this->initializerCallbackMock), $cloner);

        // setting identifiers in the proxy via reflection
        foreach ($metadata->getIdentifierFieldNames() as $idField) {
            $prop = $metadata->getReflectionClass()->getProperty($idField);
            $prop->setAccessible(true);
            $prop->setValue($this->lazyObject, $identifier[$idField]);
        }

        $this->assertFalse($this->lazyObject->__isInitialized());
    }

    public function testFetchingPublicIdentifierDoesNotCauseLazyLoading()
    {
        $this->configureInitializerMock(0);

        $this->assertSame('publicIdentifierFieldValue', $this->lazyObject->publicIdentifierField);
    }

    public function testFetchingIdentifiersViaPublicGetterDoesNotCauseLazyLoading()
    {
        $this->configureInitializerMock(0);

        $this->assertSame('protectedIdentifierFieldValue', $this->lazyObject->getProtectedIdentifierField());
    }

    public function testCallingMethodCausesLazyLoading()
    {
        $this->configureInitializerMock(
            1,
            array($this->lazyObject, 'testInitializationTriggeringMethod', array()),
            function (Proxy $proxy) {
                $proxy->__setInitializer(null);
            }
        );

        $this->lazyObject->testInitializationTriggeringMethod();
        $this->lazyObject->testInitializationTriggeringMethod();
    }

    public function testFetchingPublicFieldsCausesLazyLoading()
    {
        $test = $this;
        $this->configureInitializerMock(
            1,
            array($this->lazyObject, '__get', array('publicPersistentField')),
            function () use ($test) {
                $test->setProxyValue('publicPersistentField', 'loadedValue');
            }
        );

        $this->assertSame('loadedValue', $this->lazyObject->publicPersistentField);
        $this->assertSame('loadedValue', $this->lazyObject->publicPersistentField);
    }

    public function testFetchingPublicAssociationCausesLazyLoading()
    {
        $test = $this;
        $this->configureInitializerMock(
            1,
            array($this->lazyObject, '__get', array('publicAssociation')),
            function () use ($test) {
                $test->setProxyValue('publicAssociation', 'loadedAssociation');
            }
        );

        $this->assertSame('loadedAssociation', $this->lazyObject->publicAssociation);
        $this->assertSame('loadedAssociation', $this->lazyObject->publicAssociation);
    }

    public function testFetchingProtectedAssociationViaPublicGetterCausesLazyLoading()
    {
        $this->configureInitializerMock(
            1,
            array($this->lazyObject, 'getProtectedAssociation', array()),
            function (Proxy $proxy) {
                $proxy->__setInitializer(null);
            }
        );

        $this->assertSame('protectedAssociationValue', $this->lazyObject->getProtectedAssociation());
        $this->assertSame('protectedAssociationValue', $this->lazyObject->getProtectedAssociation());
    }

    public function testLazyLoadingTriggeredOnlyAtFirstPublicPropertyRead()
    {
        $test = $this;
        $this->configureInitializerMock(
            1,
            array($this->lazyObject, '__get', array('publicPersistentField')),
            function () use ($test) {
                $test->setProxyValue('publicPersistentField', 'loadedValue');
                $test->setProxyValue('publicAssociation', 'publicAssociationValue');
            }
        );

        $this->assertSame('loadedValue', $this->lazyObject->publicPersistentField);
        $this->assertSame('publicAssociationValue', $this->lazyObject->publicAssociation);
    }

    public function testNoticeWhenReadingNonExistentPublicProperties()
    {
        $this->configureInitializerMock(0);

        $class = get_class($this->lazyObject);
        $this->setExpectedException(
            'PHPUnit_Framework_Error_Notice',
            'Undefined property: ' . $class . '::$non_existing_property'
        );

        $this->lazyObject->non_existing_property;
    }

    public function testFalseWhenCheckingNonExistentProperty()
    {
        $this->configureInitializerMock(0);

        $this->assertFalse(isset($this->lazyObject->non_existing_property));
    }

    public function testNoErrorWhenSettingNonExistentProperty()
    {
        $this->configureInitializerMock(0);

        $this->lazyObject->non_existing_property = 'now has a value';
        $this->assertSame('now has a value', $this->lazyObject->non_existing_property);
    }

    public function testCloningCallsClonerWithClonedObject()
    {
        $lazyObject = $this->lazyObject;
        $test = $this;
        $cb = $this->getMock('stdClass', array('cb'));
        $cb
            ->expects($this->once())
            ->method('cb')
            ->will($this->returnCallback(function (LazyLoadableObject $proxy) use ($lazyObject, $test) {
                /* @var $proxy LazyLoadableObject|Proxy */
                $test->assertNotSame($proxy, $lazyObject);
                $proxy->__setInitializer(null);
                $proxy->publicAssociation = 'clonedAssociation';
            }));

        $this->lazyObject->__setCloner($this->getClosure(array($cb, 'cb')));

        $cloned = clone $this->lazyObject;
        $this->assertSame('clonedAssociation', $cloned->publicAssociation);
        $this->assertNotSame($cloned, $lazyObject, 'a clone of the lazy object is retrieved');
    }

    public function testFetchingTransientPropertiesWillNotTriggerLazyLoading()
    {
        $this->configureInitializerMock(0);

        $this->assertSame(
            'publicTransientFieldValue',
            $this->lazyObject->publicTransientField,
            'fetching public transient field won\'t trigger lazy loading'
        );
        $property = $this
            ->lazyLoadableObjectMetadata
            ->getReflectionClass()
            ->getProperty('protectedTransientField');
        $property->setAccessible(true);
        $this->assertSame(
            'protectedTransientFieldValue',
            $property->getValue($this->lazyObject),
            'fetching protected transient field via reflection won\'t trigger lazy loading'
        );
    }

    /**
     * Provided to guarantee backwards compatibility
     */
    public function testLoadProxyMethod()
    {
        $this->configureInitializerMock(2, array($this->lazyObject, '__load', array()));

        $this->lazyObject->__load();
        $this->lazyObject->__load();
    }

    public function testLoadingWithPersisterWillBeTriggeredOnlyOnce()
    {
        $this
            ->proxyLoader
            ->expects($this->once())
            ->method('load')
            ->with(
                array(
                    'publicIdentifierField' => 'publicIdentifierFieldValue',
                    'protectedIdentifierField' => 'protectedIdentifierFieldValue',
                ),
                $this->lazyObject
            )
            ->will($this->returnCallback(function ($id, LazyLoadableObject $lazyObject) {
                // setting a value to verify that the persister can actually set something in the object
                $lazyObject->publicAssociation = $id['publicIdentifierField'] . '-test';
                return true;
            }));
        $this->lazyObject->__setInitializer($this->getSuggestedInitializerImplementation());

        $this->lazyObject->__load();
        $this->lazyObject->__load();
        $this->assertSame('publicIdentifierFieldValue-test', $this->lazyObject->publicAssociation);
    }

    public function testFailedLoadingWillThrowException()
    {
        $this->proxyLoader->expects($this->any())->method('load')->will($this->returnValue(null));
        $this->setExpectedException('UnexpectedValueException');
        $this->lazyObject->__setInitializer($this->getSuggestedInitializerImplementation());

        $this->lazyObject->__load();
    }

    public function testCloningWithPersister()
    {
        $this->lazyObject->publicTransientField = 'should-not-change';
        $this
            ->proxyLoader
            ->expects($this->exactly(2))
            ->method('load')
            ->with(array(
                'publicIdentifierField'    => 'publicIdentifierFieldValue',
                'protectedIdentifierField' => 'protectedIdentifierFieldValue',
            ))
            ->will($this->returnCallback(function () {
                $blueprint = new LazyLoadableObject();
                $blueprint->publicPersistentField = 'checked-persistent-field';
                $blueprint->publicAssociation     = 'checked-association-field';
                $blueprint->publicTransientField  = 'checked-transient-field';

                return $blueprint;
            }));

        $firstClone = clone $this->lazyObject;
        $this->assertSame(
            'checked-persistent-field',
            $firstClone->publicPersistentField,
            'Persistent fields are cloned correctly'
        );
        $this->assertSame(
            'checked-association-field',
            $firstClone->publicAssociation,
            'Associations are cloned correctly'
        );
        $this->assertSame(
            'should-not-change',
            $firstClone->publicTransientField,
            'Transient fields are not overwritten'
        );

        $secondClone = clone $this->lazyObject;
        $this->assertSame(
            'checked-persistent-field',
            $secondClone->publicPersistentField,
            'Persistent fields are cloned correctly'
        );
        $this->assertSame(
            'checked-association-field',
            $secondClone->publicAssociation,
            'Associations are cloned correctly'
        );
        $this->assertSame(
            'should-not-change',
            $secondClone->publicTransientField,
            'Transient fields are not overwritten'
        );

        // those should not trigger lazy loading
        $firstClone->__load();
        $secondClone->__load();
    }

    public function testNotInitializedProxyUnserialization()
    {
        $this->configureInitializerMock();

        $serialized = serialize($this->lazyObject);
        /* @var $unserialized LazyLoadableObject|Proxy */
        $unserialized = unserialize($serialized);
        $reflClass = $this->lazyLoadableObjectMetadata->getReflectionClass();

        $this->assertFalse($unserialized->__isInitialized(), 'serialization didn\'t cause intialization');

        // Checking identifiers
        $this->assertSame('publicIdentifierFieldValue', $unserialized->publicIdentifierField, 'identifiers are kept');
        $protectedIdentifierField = $reflClass->getProperty('protectedIdentifierField');
        $protectedIdentifierField->setAccessible(true);
        $this->assertSame(
            'protectedIdentifierFieldValue',
            $protectedIdentifierField->getValue($unserialized),
            'identifiers are kept'
        );

        // Checking transient fields
        $this->assertSame(
            'publicTransientFieldValue',
            $unserialized->publicTransientField,
            'transient fields are kept'
        );
        $protectedTransientField = $reflClass->getProperty('protectedTransientField');
        $protectedTransientField->setAccessible(true);
        $this->assertSame(
            'protectedTransientFieldValue',
            $protectedTransientField->getValue($unserialized),
            'transient fields are kept'
        );

        // Checking persistent fields
        $this->assertSame(
            'publicPersistentFieldValue',
            $unserialized->publicPersistentField,
            'persistent fields are kept'
        );
        $protectedPersistentField = $reflClass->getProperty('protectedPersistentField');
        $protectedPersistentField->setAccessible(true);
        $this->assertSame(
            'protectedPersistentFieldValue',
            $protectedPersistentField->getValue($unserialized),
            'persistent fields are kept'
        );

        // Checking associations
        $this->assertSame('publicAssociationValue', $unserialized->publicAssociation, 'associations are kept');
        $protectedAssociationField = $reflClass->getProperty('protectedAssociation');
        $protectedAssociationField->setAccessible(true);
        $this->assertSame(
            'protectedAssociationValue',
            $protectedAssociationField->getValue($unserialized),
            'associations are kept'
        );
    }

    public function testInitializedProxyUnserialization()
    {
        // persister will retrieve the lazy object itself, so that we don't have to re-define all field values
        $this->proxyLoader->expects($this->once())->method('load')->will($this->returnValue($this->lazyObject));
        $this->lazyObject->__setInitializer($this->getSuggestedInitializerImplementation());
        $this->lazyObject->__load();

        $serialized   = serialize($this->lazyObject);
        $reflClass    = $this->lazyLoadableObjectMetadata->getReflectionClass();
        /* @var $unserialized LazyLoadableObject|Proxy */
        $unserialized = unserialize($serialized);

        $this->assertTrue($unserialized->__isInitialized(), 'serialization didn\'t cause intialization');

        // Checking transient fields
        $this->assertSame(
            'publicTransientFieldValue',
            $unserialized->publicTransientField,
            'transient fields are kept'
        );
        $protectedTransientField = $reflClass->getProperty('protectedTransientField');
        $protectedTransientField->setAccessible(true);
        $this->assertSame(
            'protectedTransientFieldValue',
            $protectedTransientField->getValue($unserialized),
            'transient fields are kept'
        );

        // Checking persistent fields
        $this->assertSame(
            'publicPersistentFieldValue',
            $unserialized->publicPersistentField,
            'persistent fields are kept'
        );
        $protectedPersistentField = $reflClass->getProperty('protectedPersistentField');
        $protectedPersistentField->setAccessible(true);
        $this->assertSame(
            'protectedPersistentFieldValue',
            $protectedPersistentField->getValue($unserialized),
            'persistent fields are kept'
        );

        // Checking identifiers
        $this->assertSame(
            'publicIdentifierFieldValue',
            $unserialized->publicIdentifierField,
            'identifiers are kept'
        );
        $protectedIdentifierField = $reflClass->getProperty('protectedIdentifierField');
        $protectedIdentifierField->setAccessible(true);
        $this->assertSame(
            'protectedIdentifierFieldValue',
            $protectedIdentifierField->getValue($unserialized),
            'identifiers are kept'
        );

        // Checking associations
        $this->assertSame('publicAssociationValue', $unserialized->publicAssociation, 'associations are kept');
        $protectedAssociationField = $reflClass->getProperty('protectedAssociation');
        $protectedAssociationField->setAccessible(true);
        $this->assertSame(
            'protectedAssociationValue',
            $protectedAssociationField->getValue($unserialized),
            'associations are kept'
        );
    }

    public function testInitializationRestoresDefaultPublicLazyLoadedFieldValues()
    {
        // setting noop persister
        $this->proxyLoader->expects($this->once())->method('load')->will($this->returnValue($this->lazyObject));
        $this->lazyObject->__setInitializer($this->getSuggestedInitializerImplementation());

        $this->assertSame(
            'publicPersistentFieldValue',
            $this->lazyObject->publicPersistentField,
            'Persistent field is restored to default value'
        );
        $this->assertSame(
            'publicAssociationValue',
            $this->lazyObject->publicAssociation,
            'Association is restored to default value'
        );
    }

    public function testSettingPublicFieldsCausesLazyLoading()
    {
        $test = $this;
        $this->configureInitializerMock(
            1,
            array($this->lazyObject, '__set', array('publicPersistentField', 'newPublicPersistentFieldValue')),
            function () use ($test) {
                $test->setProxyValue('publicPersistentField', 'overrideValue');
                $test->setProxyValue('publicAssociation', 'newAssociationValue');
            }
        );

        $this->lazyObject->publicPersistentField = 'newPublicPersistentFieldValue';
        $this->assertSame('newPublicPersistentFieldValue', $this->lazyObject->publicPersistentField);
        $this->assertSame('newAssociationValue', $this->lazyObject->publicAssociation);
    }

    public function testSettingPublicAssociationCausesLazyLoading()
    {
        $test = $this;
        $this->configureInitializerMock(
            1,
            array($this->lazyObject, '__set', array('publicAssociation', 'newPublicAssociationValue')),
            function () use ($test) {
                $test->setProxyValue('publicPersistentField', 'newPublicPersistentFieldValue');
                $test->setProxyValue('publicAssociation', 'overrideValue');
            }
        );

        $this->lazyObject->publicAssociation = 'newPublicAssociationValue';
        $this->assertSame('newPublicAssociationValue', $this->lazyObject->publicAssociation);
        $this->assertSame('newPublicPersistentFieldValue', $this->lazyObject->publicPersistentField);
    }

    public function testCheckingPublicFieldsCausesLazyLoading()
    {
        $test = $this;
        $this->configureInitializerMock(
            1,
            array($this->lazyObject, '__isset', array('publicPersistentField')),
            function () use ($test) {
                $test->setProxyValue('publicPersistentField', null);
                $test->setProxyValue('publicAssociation', 'setPublicAssociation');
            }
        );

        $this->assertFalse(isset($this->lazyObject->publicPersistentField));
        $this->assertNull($this->lazyObject->publicPersistentField);
        $this->assertTrue(isset($this->lazyObject->publicAssociation));
        $this->assertSame('setPublicAssociation', $this->lazyObject->publicAssociation);
    }

    public function testCheckingPublicAssociationCausesLazyLoading()
    {
        $test = $this;
        $this->configureInitializerMock(
            1,
            array($this->lazyObject, '__isset', array('publicAssociation')),
            function () use ($test) {
                $test->setProxyValue('publicPersistentField', 'newPersistentFieldValue');
                $test->setProxyValue('publicAssociation', 'setPublicAssociation');
            }
        );

        $this->assertTrue(isset($this->lazyObject->publicAssociation));
        $this->assertSame('setPublicAssociation', $this->lazyObject->publicAssociation);
        $this->assertTrue(isset($this->lazyObject->publicPersistentField));
        $this->assertSame('newPersistentFieldValue', $this->lazyObject->publicPersistentField);
    }

    /**
     * Converts a given callable into a closure
     *
     * @param  callable $callable
     * @return \Closure
     */
    public function getClosure($callable) {
        return function () use ($callable) {
            call_user_func_array($callable, func_get_args());
        };
    }

    /**
     * Configures the current initializer callback mock with provided matcher params
     *
     * @param int $expectedCallCount the number of invocations to be expected. If a value< 0 is provided, `any` is used
     * @param array $callParamsMatch an ordered array of parameters to be expected
     * @param callable $callbackClosure a return callback closure
     *
     * @return \PHPUnit_Framework_MockObject_MockObject|
     */
    protected function configureInitializerMock(
        $expectedCallCount = 0,
        array $callParamsMatch = null,
        \Closure $callbackClosure = null
    ) {
        if (!$expectedCallCount) {
            $invocationCountMatcher = $this->exactly((int) $expectedCallCount);
        } else {
            $invocationCountMatcher = $expectedCallCount < 0 ? $this->any() : $this->exactly($expectedCallCount);
        }

        $invocationMocker = $this->initializerCallbackMock->expects($invocationCountMatcher)->method('__invoke');

        if (null !== $callParamsMatch) {
            call_user_func_array(array($invocationMocker, 'with'), $callParamsMatch);
        }

        if ($callbackClosure) {
            $invocationMocker->will($this->returnCallback($callbackClosure));
        }
    }

    /**
     * Sets a value in the current proxy object without triggering lazy loading through `__set`
     *
     * @link https://bugs.php.net/bug.php?id=63463
     *
     * @param string $property
     * @param mixed $value
     */
    public function setProxyValue($property, $value)
    {
        $reflectionProperty = new \ReflectionProperty($this->lazyObject, $property);
        $initializer        = $this->lazyObject->__getInitializer();

        // disabling initializer since setting `publicPersistentField` triggers `__set`/`__get`
        $this->lazyObject->__setInitializer(null);
        $reflectionProperty->setValue($this->lazyObject, $value);
        $this->lazyObject->__setInitializer($initializer);
    }

    /**
     * Retrieves the suggested implementation of an initializer that proxy factories in O*M
     * are currently following, and that should be used to initialize the current proxy object
     *
     * @return \Closure
     */
    protected function getSuggestedInitializerImplementation()
    {
        $loader     = $this->proxyLoader;
        $identifier = $this->identifier;

        return function (LazyLoadableObject $proxy) use ($loader, $identifier) {
            /* @var $proxy LazyLoadableObject|Proxy */
            $proxy->__setInitializer(null);
            $proxy->__setCloner(null);


            if ($proxy->__isInitialized()) {
                return;
            }

            $properties = $proxy->__getLazyProperties();

            foreach ($properties as $propertyName => $property) {
                if (!isset($proxy->$propertyName)) {
                    $proxy->$propertyName = $properties[$propertyName];
                }
            }

            $proxy->__setInitialized(true);

            if (method_exists($proxy, '__wakeup')) {
                $proxy->__wakeup();
            }

            if (null === $loader->load($identifier, $proxy)) {
                throw new \UnexpectedValueException('Couldn\'t load');
            }
        };
    }
}