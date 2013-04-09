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

use ReflectionMethod;
use ReflectionException;

class StaticReflectionMethod extends ReflectionMethod
{
    /**
     * The PSR-0 parser object.
     *
     * @var StaticReflectionParser
     */
    protected $staticReflectionParser;

    /**
     * The name of the method.
     *
     * @var string
     */
    protected $methodName;

    public function __construct(StaticReflectionParser $staticReflectionParser, $methodName)
    {
        $this->staticReflectionParser = $staticReflectionParser;
        $this->methodName = $methodName;
    }
    public function getName()
    {
        return $this->methodName;
    }
    protected function getStaticReflectionParser()
    {
        return $this->staticReflectionParser->getStaticReflectionParserForDeclaringClass('method', $this->methodName);
    }
    public function getDeclaringClass()
    {
        return $this->getStaticReflectionParser()->getReflectionClass();
    }
    public function getNamespaceName()
    {
        return $this->getStaticReflectionParser()->getNamespaceName();
    }
    public function getDocComment()
    {
        return $this->getStaticReflectionParser()->getDocComment('method', $this->methodName);
    }
    public function getUseStatements()
    {
        return $this->getStaticReflectionParser()->getUseStatements();
    }
    public static function export($class, $name, $return = false) { throw new ReflectionException('Method not implemented'); }
    public function getClosure($object) { throw new ReflectionException('Method not implemented'); }
    public function getModifiers() { throw new ReflectionException('Method not implemented'); }
    public function getPrototype() { throw new ReflectionException('Method not implemented'); }
    public function invoke($object, $parameter = NULL) { throw new ReflectionException('Method not implemented'); }
    public function invokeArgs($object, array $args) { throw new ReflectionException('Method not implemented'); }
    public function isAbstract() { throw new ReflectionException('Method not implemented'); }
    public function isConstructor() { throw new ReflectionException('Method not implemented'); }
    public function isDestructor() { throw new ReflectionException('Method not implemented'); }
    public function isFinal() { throw new ReflectionException('Method not implemented'); }
    public function isPrivate() { throw new ReflectionException('Method not implemented'); }
    public function isProtected() { throw new ReflectionException('Method not implemented'); }
    public function isPublic() { throw new ReflectionException('Method not implemented'); }
    public function isStatic() { throw new ReflectionException('Method not implemented'); }
    public function setAccessible($accessible) { throw new ReflectionException('Method not implemented'); }
    public function __toString() { throw new ReflectionException('Method not implemented'); }
    public function getClosureThis() { throw new ReflectionException('Method not implemented'); }
    public function getEndLine() { throw new ReflectionException('Method not implemented'); }
    public function getExtension() { throw new ReflectionException('Method not implemented'); }
    public function getExtensionName() { throw new ReflectionException('Method not implemented'); }
    public function getFileName() { throw new ReflectionException('Method not implemented'); }
    public function getNumberOfParameters() { throw new ReflectionException('Method not implemented'); }
    public function getNumberOfRequiredParameters() { throw new ReflectionException('Method not implemented'); }
    public function getParameters() { throw new ReflectionException('Method not implemented'); }
    public function getShortName() { throw new ReflectionException('Method not implemented'); }
    public function getStartLine() { throw new ReflectionException('Method not implemented'); }
    public function getStaticVariables() { throw new ReflectionException('Method not implemented'); }
    public function inNamespace() { throw new ReflectionException('Method not implemented'); }
    public function isClosure() { throw new ReflectionException('Method not implemented'); }
    public function isDeprecated() { throw new ReflectionException('Method not implemented'); }
    public function isInternal() { throw new ReflectionException('Method not implemented'); }
    public function isUserDefined() { throw new ReflectionException('Method not implemented'); }
    public function returnsReference() { throw new ReflectionException('Method not implemented'); }
}
