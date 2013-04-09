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
 * <http://www.phpdoctrine.org>.
 */

namespace Doctrine\Common\Persistence\Mapping;

/**
 * A MappingException indicates that something is wrong with the mapping setup.
 *
 * @since 2.2
 */
class MappingException extends \Exception
{
    /**
     *
     * @param string $className
     * @param array $namespaces
     *
     * @return MappingException
     */
    public static function classNotFoundInNamespaces($className, $namespaces)
    {
        return new self("The class '" . $className . "' was not found in the ".
            "chain configured namespaces " . implode(", ", $namespaces));
    }

    /**
     * @return MappingException
     */
    public static function pathRequired()
    {
        return new self("Specifying the paths to your entities is required ".
            "in the AnnotationDriver to retrieve all class names.");
    }

    /**
     * @param string|null $path
     * @return MappingException
     */
    public static function fileMappingDriversRequireConfiguredDirectoryPath($path = null)
    {
        if ( ! empty($path)) {
            $path = '[' . $path . ']';
        }

        return new self(
            'File mapping drivers must have a valid directory path, ' .
            'however the given path ' . $path . ' seems to be incorrect!'
        );
    }

    /**
     * @param string $entityName
     * @param string $fileName
     * @return MappingException
     */
    public static function mappingFileNotFound($entityName, $fileName)
    {
        return new self("No mapping file found named '$fileName' for class '$entityName'.");
    }

    /**
     * @param string $entityName
     * @param string $fileName
     * @return MappingException
     */
    public static function invalidMappingFile($entityName, $fileName)
    {
        return new self("Invalid mapping file '$fileName' for class '$entityName'.");
    }
}
