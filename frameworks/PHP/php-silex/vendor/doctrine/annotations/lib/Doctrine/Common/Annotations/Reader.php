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
 * Interface for annotation readers.
 *
 * @author Johannes M. Schmitt <schmittjoh@gmail.com>
 */
interface Reader
{
    /**
     * @param \ReflectionClass $class
     * @return mixed
     */
    function getClassAnnotations(\ReflectionClass $class);

    /**
     * @param \ReflectionClass $class
     * @param string $annotationName
     * @return mixed
     */
    function getClassAnnotation(\ReflectionClass $class, $annotationName);

    /**
     * @param \ReflectionMethod $method
     * @return mixed
     */
    function getMethodAnnotations(\ReflectionMethod $method);

    /**
     * @param \ReflectionMethod $method
     * @param string $annotationName
     * @return mixed
     */
    function getMethodAnnotation(\ReflectionMethod $method, $annotationName);

    /**
     * @param \ReflectionProperty $property
     * @return mixed
     */
    function getPropertyAnnotations(\ReflectionProperty $property);

    /**
     * @param \ReflectionProperty $property
     * @param string $annotationName
     * @return mixed
     */
    function getPropertyAnnotation(\ReflectionProperty $property, $annotationName);
}
