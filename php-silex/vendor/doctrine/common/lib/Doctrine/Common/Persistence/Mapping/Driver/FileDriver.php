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

namespace Doctrine\Common\Persistence\Mapping\Driver;

use Doctrine\Common\Persistence\Mapping\MappingException;

/**
 * Base driver for file-based metadata drivers.
 *
 * A file driver operates in a mode where it loads the mapping files of individual
 * classes on demand. This requires the user to adhere to the convention of 1 mapping
 * file per class and the file names of the mapping files must correspond to the full
 * class name, including namespace, with the namespace delimiters '\', replaced by dots '.'.
 *
 * @license     http://www.opensource.org/licenses/lgpl-license.php LGPL
 * @link        www.doctrine-project.com
 * @since       2.2
 * @author      Benjamin Eberlei <kontakt@beberlei.de>
 * @author      Guilherme Blanco <guilhermeblanco@hotmail.com>
 * @author      Jonathan H. Wage <jonwage@gmail.com>
 * @author      Roman Borschel <roman@code-factory.org>
 */
abstract class FileDriver implements MappingDriver
{
    /**
     * @var FileLocator
     */
    protected $locator;

    /**
     * @var array
     */
    protected $classCache;

    /**
     * @var string
     */
    protected $globalBasename;

    /**
     * Initializes a new FileDriver that looks in the given path(s) for mapping
     * documents and operates in the specified operating mode.
     *
     * @param string|array|FileLocator $locator A FileLocator or one/multiple paths where mapping documents can be found.
     * @param string $fileExtension
     */
    public function __construct($locator, $fileExtension = null)
    {
        if ($locator instanceof FileLocator) {
            $this->locator = $locator;
        } else {
            $this->locator = new DefaultFileLocator((array)$locator, $fileExtension);
        }
    }

    /**
     * Set global basename
     *
     * @param string $file
     */
    public function setGlobalBasename($file)
    {
        $this->globalBasename = $file;
    }

    /**
     * Retrieve global basename
     *
     * @return string
     */
    public function getGlobalBasename()
    {
        return $this->globalBasename;
    }

    /**
     * Get the element of schema meta data for the class from the mapping file.
     * This will lazily load the mapping file if it is not loaded yet
     *
     * @param string $className
     *
     * @throws MappingException
     * @return array The element of schema meta data
     */
    public function getElement($className)
    {
        if ($this->classCache === null) {
            $this->initialize();
        }

        if (isset($this->classCache[$className])) {
            return $this->classCache[$className];
        }

        $result = $this->loadMappingFile($this->locator->findMappingFile($className));
        if (!isset($result[$className])) {
            throw MappingException::invalidMappingFile($className, str_replace('\\', '.', $className) . $this->locator->getFileExtension());
        }

        return $result[$className];
    }

    /**
     * Whether the class with the specified name should have its metadata loaded.
     * This is only the case if it is either mapped as an Entity or a
     * MappedSuperclass.
     *
     * @param string $className
     * @return boolean
     */
    public function isTransient($className)
    {
        if ($this->classCache === null) {
            $this->initialize();
        }

        if (isset($this->classCache[$className])) {
            return false;
        }

        return !$this->locator->fileExists($className);
    }

    /**
     * Gets the names of all mapped classes known to this driver.
     *
     * @return array The names of all mapped classes known to this driver.
     */
    public function getAllClassNames()
    {
        if ($this->classCache === null) {
            $this->initialize();
        }

        $classNames = (array)$this->locator->getAllClassNames($this->globalBasename);
        if ($this->classCache) {
            $classNames = array_merge(array_keys($this->classCache), $classNames);
        }
        return $classNames;
    }

    /**
     * Loads a mapping file with the given name and returns a map
     * from class/entity names to their corresponding file driver elements.
     *
     * @param string $file The mapping file to load.
     * @return array
     */
    abstract protected function loadMappingFile($file);

    /**
     * Initialize the class cache from all the global files.
     *
     * Using this feature adds a substantial performance hit to file drivers as
     * more metadata has to be loaded into memory than might actually be
     * necessary. This may not be relevant to scenarios where caching of
     * metadata is in place, however hits very hard in scenarios where no
     * caching is used.
     *
     * @return void
     */
    protected function initialize()
    {
        $this->classCache = array();
        if (null !== $this->globalBasename) {
            foreach ($this->locator->getPaths() as $path) {
                $file = $path.'/'.$this->globalBasename.$this->locator->getFileExtension();
                if (is_file($file)) {
                    $this->classCache = array_merge(
                        $this->classCache,
                        $this->loadMappingFile($file)
                    );
                }
            }
        }
    }

    /**
     * Retrieve the locator used to discover mapping files by className
     *
     * @return FileLocator
     */
    public function getLocator()
    {
        return $this->locator;
    }

    /**
     * Set the locator used to discover mapping files by className
     *
     * @param FileLocator $locator
     */
    public function setLocator(FileLocator $locator)
    {
        $this->locator = $locator;
    }
}
