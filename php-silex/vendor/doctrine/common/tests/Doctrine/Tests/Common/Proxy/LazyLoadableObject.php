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

/**
 * Test asset representing a lazy loadable object
 *
 * @author Marco Pivetta <ocramius@gmail.com>
 * @since  2.4
 */
class LazyLoadableObject
{
    /**
     * @var string
     */
    public $publicIdentifierField;

    /**
     * @var string
     */
    protected $protectedIdentifierField;

    /**
     * @var string
     */
    public $publicTransientField            = 'publicTransientFieldValue';

    /**
     * @var string
     */
    protected $protectedTransientField      = 'protectedTransientFieldValue';

    /**
     * @var string
     */
    public $publicPersistentField           = 'publicPersistentFieldValue';

    /**
     * @var string
     */
    protected $protectedPersistentField     = 'protectedPersistentFieldValue';

    /**
     * @var string
     */
    public $publicAssociation               = 'publicAssociationValue';

    /**
     * @var string
     */
    protected $protectedAssociation         = 'protectedAssociationValue';

    /**
     * @return string
     */
    public function getProtectedIdentifierField()
    {
        return $this->protectedIdentifierField;
    }

    /**
     * @return string
     */
    public function testInitializationTriggeringMethod()
    {
        return 'testInitializationTriggeringMethod';
    }

    /**
     * @return string
     */
    public function getProtectedAssociation()
    {
        return $this->protectedAssociation;
    }

    /**
     * @param \stdClass $param
     */
    public function publicTypeHintedMethod(\stdClass $param)
    {
    }

    /**
     *
     */
    public function &byRefMethod()
    {
    }

    /**
     * @param mixed $thisIsNotByRef
     * @param &mixed $thisIsByRef
     */
    public function byRefParamMethod($thisIsNotByRef, &$thisIsByRef)
    {
    }
}
