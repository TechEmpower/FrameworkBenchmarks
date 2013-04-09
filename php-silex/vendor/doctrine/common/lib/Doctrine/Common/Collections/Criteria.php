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

namespace Doctrine\Common\Collections;

use Doctrine\Common\Collections\Expr\Expression;
use Doctrine\Common\Collections\Expr\CompositeExpression;

/**
 * Criteria for filtering Selectable collections.
 *
 * @author Benjamin Eberlei <kontakt@beberlei.de>
 * @since 2.3
 */
class Criteria
{
    /**
     * @var string
     */
    const ASC  = 'ASC';

    /**
     * @var string
     */
    const DESC = 'DESC';

    /**
     * @var \Doctrine\Common\Collections\ExpressionBuilder
     */
    private static $expressionBuilder;

    /**
     * @var \Doctrine\Common\Collections\Expr\Expression
     */
    private $expression;

    /**
     * @var array|null
     */
    private $orderings;

    /**
     * @var int
     */
    private $firstResult;

    /**
     * @var int
     */
    private $maxResults;

    /**
     * Creates an instance of the class.
     *
     * @return Criteria
     */
    public static function create()
    {
        return new static();
    }

    /**
     * Return the expression builder.
     *
     * @return \Doctrine\Common\Collections\ExpressionBuilder
     */
    public static function expr()
    {
        if (self::$expressionBuilder === null) {
            self::$expressionBuilder = new ExpressionBuilder();
        }
        return self::$expressionBuilder;
    }

    /**
     * Construct new criteria
     *
     * @param Expression $expression
     * @param array $orderings
     * @param int $firstResult
     * @param int $maxResults
     */
    public function __construct(Expression $expression = null, array $orderings = null, $firstResult = null, $maxResults = null)
    {
        $this->expression  = $expression;
        $this->orderings   = $orderings;
        $this->firstResult = $firstResult;
        $this->maxResults  = $maxResults;
    }

    /**
     * Set the where expression to evaluate when this criteria is searched for.
     *
     * @param Expression
     * @return Criteria
     */
    public function where(Expression $expression)
    {
        $this->expression = $expression;
        return $this;
    }

    /**
     * Append the where expression to evaluate when this criteria is searched for
     * using an AND with previous expression.
     *
     * @param Expression
     * @return Criteria
     */
    public function andWhere(Expression $expression)
    {
        if ($this->expression === null) {
            return $this->where($expression);
        }

        $this->expression = new CompositeExpression(CompositeExpression::TYPE_AND, array(
            $this->expression, $expression
        ));

        return $this;
    }

    /**
     * Append the where expression to evaluate when this criteria is searched for
     * using an OR with previous expression.
     *
     * @param Expression
     * @return Criteria
     */
    public function orWhere(Expression $expression)
    {
        if ($this->expression === null) {
            return $this->where($expression);
        }

        $this->expression = new CompositeExpression(CompositeExpression::TYPE_OR, array(
            $this->expression, $expression
        ));

        return $this;
    }

    /**
     * Get the expression attached to this criteria.
     *
     * @return Expression|null
     */
    public function getWhereExpression()
    {
        return $this->expression;
    }

    /**
     * Get current orderings of this Criteria
     *
     * @return array
     */
    public function getOrderings()
    {
        return $this->orderings;
    }

    /**
     * Set the ordering of the result of this criteria.
     *
     * Keys are field and values are the order, being either ASC or DESC.
     *
     * @see Criteria::ASC
     * @see Criteria::DESC
     *
     * @param array
     * @return Criteria
     */
    public function orderBy(array $orderings)
    {
        $this->orderings = $orderings;
        return $this;
    }

    /**
     * Get current first result option of the critera.
     *
     * @return firstResult.
     */
    public function getFirstResult()
    {
        return $this->firstResult;
    }

    /**
     * Set number of first result that this criteria should return.
     *
     * @param firstResult the value to set.
     * @return Criteria
     */
    public function setFirstResult($firstResult)
    {
        $this->firstResult = $firstResult;
        return $this;
    }

    /**
     * Get maxResults.
     *
     * @return maxResults.
     */
    public function getMaxResults()
    {
        return $this->maxResults;
    }

    /**
     * Set maxResults.
     *
     * @param maxResults the value to set.
     * @return Criteria
     */
    public function setMaxResults($maxResults)
    {
        $this->maxResults = $maxResults;
        return $this;
    }
}

