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

namespace Doctrine\Common\Collections\Expr;

/**
 * Walks an expression graph and turns it into a PHP closure.
 *
 * This closure can be used with {@Collection#filter()} and is used internally
 * by {@ArrayCollection#select()}.
 *
 * @author Benjamin Eberlei <kontakt@beberlei.de>
 * @since 2.3
 */
class ClosureExpressionVisitor extends ExpressionVisitor
{
    /**
     * Access the field of a given object. This field has to be public directly
     * or indirectly (through an accessor get* or a magic method, __get, __call).
     *
     * is*() is not supported.
     *
     * @return mixed
     */
    static public function getObjectFieldValue($object, $field)
    {
        $accessor = "get" . $field;

        if (method_exists($object, $accessor) || method_exists($object, '__call')) {
            return $object->$accessor();
        }

        if ($object instanceof \ArrayAccess) {
            return $object[$field];
        }

        return $object->$field;
    }

    /**
     * Helper for sorting arrays of objects based on multiple fields +
     * orientations.
     *
     * @param string $name
     * @param int $orientation
     * @param Closure $next
     * @return Closure
     */
    static public function sortByField($name, $orientation = 1, \Closure $next = null)
    {
        if (!$next) {
            $next = function() {
                return 0;
            };
        }

        return function ($a, $b) use ($name, $next, $orientation) {
            $aValue = ClosureExpressionVisitor::getObjectFieldValue($a, $name);
            $bValue = ClosureExpressionVisitor::getObjectFieldValue($b, $name);

            if ($aValue === $bValue) {
                return $next($a, $b);
            }

            return (($aValue > $bValue) ? 1 : -1) * $orientation;
        };
    }

    /**
     * {@inheritDoc}
     */
    public function walkComparison(Comparison $comparison)
    {
        $field = $comparison->getField();
        $value = $comparison->getValue()->getValue(); // shortcut for walkValue()

        switch ($comparison->getOperator()) {
            case Comparison::EQ:
            case Comparison::IS:
                return function ($object) use ($field, $value) {
                    return ClosureExpressionVisitor::getObjectFieldValue($object, $field) === $value;
                };

            case Comparison::NEQ:
                return function ($object) use ($field, $value) {
                    return ClosureExpressionVisitor::getObjectFieldValue($object, $field) !== $value;
                };

            case Comparison::LT:
                return function ($object) use ($field, $value) {
                    return ClosureExpressionVisitor::getObjectFieldValue($object, $field) < $value;
                };

            case Comparison::LTE:
                return function ($object) use ($field, $value) {
                    return ClosureExpressionVisitor::getObjectFieldValue($object, $field) <= $value;
                };

            case Comparison::GT:
                return function ($object) use ($field, $value) {
                    return ClosureExpressionVisitor::getObjectFieldValue($object, $field) > $value;
                };

            case Comparison::GTE:
                return function ($object) use ($field, $value) {
                    return ClosureExpressionVisitor::getObjectFieldValue($object, $field) >= $value;
                };

            case Comparison::IN:
                return function ($object) use ($field, $value) {
                    return in_array(ClosureExpressionVisitor::getObjectFieldValue($object, $field), $value);
                };

            case Comparison::NIN:
                return function ($object) use ($field, $value) {
                    return ! in_array(ClosureExpressionVisitor::getObjectFieldValue($object, $field), $value);
                };

            default:
                throw new \RuntimeException("Unknown comparison operator: " . $comparison->getOperator());
        }
    }

    /**
     * {@inheritDoc}
     */
    public function walkValue(Value $value)
    {
        return $value->getValue();
    }

    /**
     * {@inheritDoc}
     */
    public function walkCompositeExpression(CompositeExpression $expr)
    {
        $expressionList = array();

        foreach ($expr->getExpressionList() as $child) {
            $expressionList[] = $this->dispatch($child);
        }

        switch($expr->getType()) {
            case CompositeExpression::TYPE_AND:
                return $this->andExpressions($expressionList);

            case CompositeExpression::TYPE_OR:
                return $this->orExpressions($expressionList);

            default:
                throw new \RuntimeException("Unknown composite " . $expr->getType());
        }
    }

    private function andExpressions($expressions)
    {
        return function ($object) use ($expressions) {
            foreach ($expressions as $expression) {
                if ( ! $expression($object)) {
                    return false;
                }
            }
            return true;
        };
    }

    private function orExpressions($expressions)
    {
        return function ($object) use ($expressions) {
            foreach ($expressions as $expression) {
                if ($expression($object)) {
                    return true;
                }
            }
            return false;
        };
    }
}

