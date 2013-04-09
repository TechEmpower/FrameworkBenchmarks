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

namespace Doctrine\Common\Annotations;

/**
 * Description of AnnotationException
 *
 * @since   2.0
 * @author  Benjamin Eberlei <kontakt@beberlei.de>
 * @author  Guilherme Blanco <guilhermeblanco@hotmail.com>
 * @author  Jonathan Wage <jonwage@gmail.com>
 * @author  Roman Borschel <roman@code-factory.org>
 */
class AnnotationException extends \Exception
{
    /**
     * Creates a new AnnotationException describing a Syntax error.
     *
     * @param string $message Exception message
     * @return AnnotationException
     */
    public static function syntaxError($message)
    {
        return new self('[Syntax Error] ' . $message);
    }

    /**
     * Creates a new AnnotationException describing a Semantical error.
     *
     * @param string $message Exception message
     * @return AnnotationException
     */
    public static function semanticalError($message)
    {
        return new self('[Semantical Error] ' . $message);
    }

    /**
     * Creates a new AnnotationException describing a constant semantical error.
     *
     * @since 2.3
     * @param string $identifier
     * @param string $context
     * @return AnnotationException
     */
    public static function semanticalErrorConstants($identifier, $context = null)
    {
        return self::semanticalError(sprintf(
            "Couldn't find constant %s%s", $identifier,
            $context ? ", $context." : "."
        ));
    }

    /**
     * Creates a new AnnotationException describing an error which occurred during
     * the creation of the annotation.
     *
     * @since 2.2
     * @param string $message
     * @return AnnotationException
     */
    public static function creationError($message)
    {
        return new self('[Creation Error] ' . $message);
    }

    /**
     * Creates a new AnnotationException describing an type error of an attribute.
     *
     * @since 2.2
     * @param string $attributeName
     * @param string $annotationName
     * @param string $context
     * @param string $expected
     * @param mixed $actual
     * @return AnnotationException
     */
    public static function typeError($attributeName, $annotationName, $context, $expected, $actual)
    {
        return new self(sprintf(
            '[Type Error] Attribute "%s" of @%s declared on %s expects %s, but got %s.',
            $attributeName,
            $annotationName,
            $context,
            $expected,
            is_object($actual) ? 'an instance of '.get_class($actual) : gettype($actual)
        ));
    }

    /**
     * Creates a new AnnotationException describing an required error of an attribute.
     *
     * @since 2.2
     * @param string $attributeName
     * @param string $annotationName
     * @param string $context
     * @param string $expected
     * @return AnnotationException
     */
    public static function requiredError($attributeName, $annotationName, $context, $expected)
    {
        return new self(sprintf(
            '[Type Error] Attribute "%s" of @%s declared on %s expects %s. This value should not be null.',
            $attributeName,
            $annotationName,
            $context,
            $expected
        ));
    }
}
