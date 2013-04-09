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

use ReflectionProperty;
use ReflectionException;

class StaticReflectionProperty extends ReflectionProperty
{
    /**
     * The PSR-0 parser object.
     *
     * @var StaticReflectionParser
     */
    protected $staticReflectionParser;

    /**
     * The name of the property.
     *
     * @var string
     */
    protected $propertyName;

    public function __construct(StaticReflectionParser $staticReflectionParser, $propertyName)
    {
        $this->staticReflectionParser = $staticReflectionParser;
        $this->propertyName = $propertyName;
    }
    public function getName()
    {
        return $this->propertyName;
    }
    protected function getStaticReflectionParser()
    {
        return $this->staticReflectionParser->getStaticReflectionParserForDeclaringClass('property', $this->propertyName);
    }
    public function getDeclaringClass()
    {
        return $this->getStaticReflectionParser()->getReflectionClass();
    }
    public function getDocComment()
    {
        return $this->getStaticReflectionParser()->getDocComment('property', $this->propertyName);
    }
    public function getUseStatements()
    {
        return $this->getStaticReflectionParser()->getUseStatements();
    }
    public static function export ($class, $name, $return = false) { throw new ReflectionException('Method not implemented'); }
    public function getModifiers() { throw new ReflectionException('Method not implemented'); }
    public function getValue($object = NULL) { throw new ReflectionException('Method not implemented'); }
    public function isDefault() { throw new ReflectionException('Method not implemented'); }
    public function isPrivate() { throw new ReflectionException('Method not implemented'); }
    public function isProtected() { throw new ReflectionException('Method not implemented'); }
    public function isPublic() { throw new ReflectionException('Method not implemented'); }
    public function isStatic() { throw new ReflectionException('Method not implemented'); }
    public function setAccessible ($accessible) { throw new ReflectionException('Method not implemented'); }
    public function setValue ($object, $value = NULL) { throw new ReflectionException('Method not implemented'); }
    public function __toString() { throw new ReflectionException('Method not implemented'); }
}
