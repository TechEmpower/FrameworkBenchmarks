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

namespace Doctrine\Common\Persistence\Mapping;

use Doctrine\Common\Cache\Cache,
    Doctrine\Common\Util\ClassUtils;

/**
 * The ClassMetadataFactory is used to create ClassMetadata objects that contain all the
 * metadata mapping informations of a class which describes how a class should be mapped
 * to a relational database.
 *
 * This class was abstracted from the ORM ClassMetadataFactory
 *
 * @since   2.2
 * @author  Benjamin Eberlei <kontakt@beberlei.de>
 * @author  Guilherme Blanco <guilhermeblanco@hotmail.com>
 * @author  Jonathan Wage <jonwage@gmail.com>
 * @author  Roman Borschel <roman@code-factory.org>
 */
abstract class AbstractClassMetadataFactory implements ClassMetadataFactory
{
    /**
     * Salt used by specific Object Manager implementation.
     *
     * @var string
     */
    protected $cacheSalt = "\$CLASSMETADATA";

    /**
     * @var \Doctrine\Common\Cache\Cache
     */
    private $cacheDriver;

    /**
     * @var array
     */
    private $loadedMetadata = array();

    /**
     * @var bool
     */
    protected $initialized = false;

    /**
     * @var ReflectionService
     */
    private $reflectionService;

    /**
     * Sets the cache driver used by the factory to cache ClassMetadata instances.
     *
     * @param Doctrine\Common\Cache\Cache $cacheDriver
     */
    public function setCacheDriver(Cache $cacheDriver = null)
    {
        $this->cacheDriver = $cacheDriver;
    }

    /**
     * Gets the cache driver used by the factory to cache ClassMetadata instances.
     *
     * @return Doctrine\Common\Cache\Cache
     */
    public function getCacheDriver()
    {
        return $this->cacheDriver;
    }

    /**
     * Return an array of all the loaded metadata currently in memory.
     *
     * @return array
     */
    public function getLoadedMetadata()
    {
        return $this->loadedMetadata;
    }

    /**
     * Forces the factory to load the metadata of all classes known to the underlying
     * mapping driver.
     *
     * @return array The ClassMetadata instances of all mapped classes.
     */
    public function getAllMetadata()
    {
        if ( ! $this->initialized) {
            $this->initialize();
        }

        $driver = $this->getDriver();
        $metadata = array();
        foreach ($driver->getAllClassNames() as $className) {
            $metadata[] = $this->getMetadataFor($className);
        }

        return $metadata;
    }

    /**
     * Lazy initialization of this stuff, especially the metadata driver,
     * since these are not needed at all when a metadata cache is active.
     *
     * @return void
     */
    abstract protected function initialize();

    /**
     * Get the fully qualified class-name from the namespace alias.
     *
     * @param string $namespaceAlias
     * @param string $simpleClassName
     * @return string
     */
    abstract protected function getFqcnFromAlias($namespaceAlias, $simpleClassName);

    /**
     * Return the mapping driver implementation.
     *
     * @return \Doctrine\Common\Persistence\Mapping\Driver\MappingDriver
     */
    abstract protected function getDriver();

    /**
     * Wakeup reflection after ClassMetadata gets unserialized from cache.
     *
     * @param ClassMetadata $class
     * @param ReflectionService $reflService
     * @return void
     */
    abstract protected function wakeupReflection(ClassMetadata $class, ReflectionService $reflService);

    /**
     * Initialize Reflection after ClassMetadata was constructed.
     *
     * @param ClassMetadata $class
     * @param ReflectionService $reflService
     * @return void
     */
    abstract protected function initializeReflection(ClassMetadata $class, ReflectionService $reflService);

    /**
     * Checks whether the class metadata is an entity.
     *
     * This method should false for mapped superclasses or
     * embedded classes.
     *
     * @param ClassMetadata $class
     * @return boolean
     */
    abstract protected function isEntity(ClassMetadata $class);

    /**
     * Gets the class metadata descriptor for a class.
     *
     * @param string $className The name of the class.
     * @return \Doctrine\Common\Persistence\Mapping\ClassMetadata
     */
    public function getMetadataFor($className)
    {
        if (isset($this->loadedMetadata[$className])) {
            return $this->loadedMetadata[$className];
        }

        $realClassName = $className;

        // Check for namespace alias
        if (strpos($className, ':') !== false) {
            list($namespaceAlias, $simpleClassName) = explode(':', $className);
            $realClassName = $this->getFqcnFromAlias($namespaceAlias, $simpleClassName);
        } else {
            $realClassName = ClassUtils::getRealClass($realClassName);
        }

        if (isset($this->loadedMetadata[$realClassName])) {
            // We do not have the alias name in the map, include it
            $this->loadedMetadata[$className] = $this->loadedMetadata[$realClassName];

            return $this->loadedMetadata[$realClassName];
        }

        if ($this->cacheDriver) {
            if (($cached = $this->cacheDriver->fetch($realClassName . $this->cacheSalt)) !== false) {
                $this->loadedMetadata[$realClassName] = $cached;
                $this->wakeupReflection($cached, $this->getReflectionService());
            } else {
                foreach ($this->loadMetadata($realClassName) as $loadedClassName) {
                    $this->cacheDriver->save(
                        $loadedClassName . $this->cacheSalt, $this->loadedMetadata[$loadedClassName], null
                    );
                }
            }
        } else {
            $this->loadMetadata($realClassName);
        }

        if ($className != $realClassName) {
            // We do not have the alias name in the map, include it
            $this->loadedMetadata[$className] = $this->loadedMetadata[$realClassName];
        }

        return $this->loadedMetadata[$className];
    }

    /**
     * Checks whether the factory has the metadata for a class loaded already.
     *
     * @param string $className
     * @return boolean TRUE if the metadata of the class in question is already loaded, FALSE otherwise.
     */
    public function hasMetadataFor($className)
    {
        return isset($this->loadedMetadata[$className]);
    }

    /**
     * Sets the metadata descriptor for a specific class.
     *
     * NOTE: This is only useful in very special cases, like when generating proxy classes.
     *
     * @param string $className
     * @param ClassMetadata $class
     */
    public function setMetadataFor($className, $class)
    {
        $this->loadedMetadata[$className] = $class;
    }

    /**
     * Get array of parent classes for the given entity class
     *
     * @param string $name
     * @return array $parentClasses
     */
    protected function getParentClasses($name)
    {
        // Collect parent classes, ignoring transient (not-mapped) classes.
        $parentClasses = array();
        foreach (array_reverse($this->getReflectionService()->getParentClasses($name)) as $parentClass) {
            if ( ! $this->getDriver()->isTransient($parentClass)) {
                $parentClasses[] = $parentClass;
            }
        }
        return $parentClasses;
    }

    /**
     * Loads the metadata of the class in question and all it's ancestors whose metadata
     * is still not loaded.
     *
     * @param string $name The name of the class for which the metadata should get loaded.
     *
     * @return array
     */
    protected function loadMetadata($name)
    {
        if ( ! $this->initialized) {
            $this->initialize();
        }

        $loaded = array();

        $parentClasses = $this->getParentClasses($name);
        $parentClasses[] = $name;

        // Move down the hierarchy of parent classes, starting from the topmost class
        $parent = null;
        $rootEntityFound = false;
        $visited = array();
        $reflService = $this->getReflectionService();
        foreach ($parentClasses as $className) {
            if (isset($this->loadedMetadata[$className])) {
                $parent = $this->loadedMetadata[$className];
                if ($this->isEntity($parent)) {
                    $rootEntityFound = true;
                    array_unshift($visited, $className);
                }
                continue;
            }

            $class = $this->newClassMetadataInstance($className);
            $this->initializeReflection($class, $reflService);

            $this->doLoadMetadata($class, $parent, $rootEntityFound, $visited);

            $this->loadedMetadata[$className] = $class;

            $parent = $class;

            if ($this->isEntity($class)) {
                $rootEntityFound = true;
                array_unshift($visited, $className);
            }

            $this->wakeupReflection($class, $reflService);

            $loaded[] = $className;
        }

        return $loaded;
    }

    /**
     * Actually load the metadata from the underlying metadata
     *
     * @param ClassMetadata $class
     * @param ClassMetadata|null $parent
     * @param bool $rootEntityFound
     * @param array $nonSuperclassParents classnames all parent classes that are not marked as mapped superclasses
     * @return void
     */
    abstract protected function doLoadMetadata($class, $parent, $rootEntityFound, array $nonSuperclassParents);

    /**
     * Creates a new ClassMetadata instance for the given class name.
     *
     * @param string $className
     * @return ClassMetadata
     */
    abstract protected function newClassMetadataInstance($className);

    /**
     * Check if this class is mapped by this Object Manager + ClassMetadata configuration
     *
     * @param $class
     * @return bool
     */
    public function isTransient($class)
    {
        if ( ! $this->initialized) {
            $this->initialize();
        }

        // Check for namespace alias
        if (strpos($class, ':') !== false) {
            list($namespaceAlias, $simpleClassName) = explode(':', $class);
            $class = $this->getFqcnFromAlias($namespaceAlias, $simpleClassName);
        }

        return $this->getDriver()->isTransient($class);
    }

    /**
     * Set reflectionService.
     *
     * @param ReflectionService $reflectionService
     */
    public function setReflectionService(ReflectionService $reflectionService)
    {
        $this->reflectionService = $reflectionService;
    }

    /**
     * Get the reflection service associated with this metadata factory.
     *
     * @return ReflectionService
     */
    public function getReflectionService()
    {
        if ($this->reflectionService === null) {
            $this->reflectionService = new RuntimeReflectionService();
        }
        return $this->reflectionService;
    }
}
