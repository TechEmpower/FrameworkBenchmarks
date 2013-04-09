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

namespace Doctrine\Common\Reflection;

use ReflectionClass;
use ReflectionException;

class StaticReflectionClass extends ReflectionClass
{
    /**
     * The static reflection parser object.
     *
     * @var StaticReflectionParser
     */
    private $staticReflectionParser;

    public function __construct(StaticReflectionParser $staticReflectionParser)
    {
        $this->staticReflectionParser = $staticReflectionParser;
    }

    public function getName()
    {
        return $this->staticReflectionParser->getClassName();
    }

    public function getDocComment()
    {
        return $this->staticReflectionParser->getDocComment();
    }

    public function getNamespaceName()
    {
        return $this->staticReflectionParser->getNamespaceName();
    }

    public function getUseStatements()
    {
        return $this->staticReflectionParser->getUseStatements();
    }

    public function getMethod($name)
    {
        return $this->staticReflectionParser->getReflectionMethod($name);
    }

    public function getProperty($name)
    {
        return $this->staticReflectionParser->getReflectionProperty($name);
    }

    public static function export($argument, $return = false) { throw new ReflectionException('Method not implemented'); }
    public function getConstant($name) { throw new ReflectionException('Method not implemented'); }
    public function getConstants() { throw new ReflectionException('Method not implemented'); }
    public function getConstructor() { throw new ReflectionException('Method not implemented'); }
    public function getDefaultProperties() { throw new ReflectionException('Method not implemented'); }
    public function getEndLine() { throw new ReflectionException('Method not implemented'); }
    public function getExtension() { throw new ReflectionException('Method not implemented'); }
    public function getExtensionName() { throw new ReflectionException('Method not implemented'); }
    public function getFileName() { throw new ReflectionException('Method not implemented'); }
    public function getInterfaceNames() { throw new ReflectionException('Method not implemented'); }
    public function getInterfaces() { throw new ReflectionException('Method not implemented'); }
    public function getMethods($filter = NULL) { throw new ReflectionException('Method not implemented'); }
    public function getModifiers() { throw new ReflectionException('Method not implemented'); }
    public function getParentClass() { throw new ReflectionException('Method not implemented'); }
    public function getProperties($filter = NULL) { throw new ReflectionException('Method not implemented'); }
    public function getShortName() { throw new ReflectionException('Method not implemented'); }
    public function getStartLine() { throw new ReflectionException('Method not implemented'); }
    public function getStaticProperties() { throw new ReflectionException('Method not implemented'); }
    public function getStaticPropertyValue($name, $default = '') { throw new ReflectionException('Method not implemented'); }
    public function getTraitAliases() { throw new ReflectionException('Method not implemented'); }
    public function getTraitNames() { throw new ReflectionException('Method not implemented'); }
    public function getTraits() { throw new ReflectionException('Method not implemented'); }
    public function hasConstant($name) { throw new ReflectionException('Method not implemented'); }
    public function hasMethod($name) { throw new ReflectionException('Method not implemented'); }
    public function hasProperty($name) { throw new ReflectionException('Method not implemented'); }
    public function implementsInterface($interface) { throw new ReflectionException('Method not implemented'); }
    public function inNamespace() { throw new ReflectionException('Method not implemented'); }
    public function isAbstract() { throw new ReflectionException('Method not implemented'); }
    public function isCloneable() { throw new ReflectionException('Method not implemented'); }
    public function isFinal() { throw new ReflectionException('Method not implemented'); }
    public function isInstance($object) { throw new ReflectionException('Method not implemented'); }
    public function isInstantiable() { throw new ReflectionException('Method not implemented'); }
    public function isInterface() { throw new ReflectionException('Method not implemented'); }
    public function isInternal() { throw new ReflectionException('Method not implemented'); }
    public function isIterateable() { throw new ReflectionException('Method not implemented'); }
    public function isSubclassOf($class) { throw new ReflectionException('Method not implemented'); }
    public function isTrait() { throw new ReflectionException('Method not implemented'); }
    public function isUserDefined() { throw new ReflectionException('Method not implemented'); }
    public function newInstance($args) { throw new ReflectionException('Method not implemented'); }
    public function newInstanceArgs(array $args = array()) { throw new ReflectionException('Method not implemented'); }
    public function newInstanceWithoutConstructor() { throw new ReflectionException('Method not implemented'); }
    public function setStaticPropertyValue($name, $value) { throw new ReflectionException('Method not implemented'); }
    public function __toString() { throw new ReflectionException('Method not implemented'); }
}
