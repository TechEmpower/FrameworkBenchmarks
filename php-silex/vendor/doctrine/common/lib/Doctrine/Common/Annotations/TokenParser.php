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
 * Parses a file for namespaces/use/class declarations.
 *
 * @author Fabien Potencier <fabien@symfony.com>
 * @author Christian Kaps <christian.kaps@mohiva.com>
 */
class TokenParser
{
    /**
     * The token list.
     *
     * @var array
     */
    private $tokens;

    /**
     * The number of tokens.
     *
     * @var int
     */
    private $numTokens = 0;

    /**
     * The current array pointer.
     *
     * @var int
     */
    private $pointer = 0;

    public function __construct($contents)
    {
        $this->tokens = token_get_all($contents);
        $this->numTokens = count($this->tokens);
        $this->pointer = 0;
    }

    /**
     * Gets the next non whitespace and non comment token.
     *
     * @param $docCommentIsComment
     *     If TRUE then a doc comment is considered a comment and skipped.
     *     If FALSE then only whitespace and normal comments are skipped.
     *
     * @return array The token if exists, null otherwise.
     */
    public function next($docCommentIsComment = TRUE)
    {
        for ($i = $this->pointer; $i < $this->numTokens; $i++) {
            $this->pointer++;
            if ($this->tokens[$i][0] === T_WHITESPACE ||
                $this->tokens[$i][0] === T_COMMENT ||
                ($docCommentIsComment && $this->tokens[$i][0] === T_DOC_COMMENT)) {

                continue;
            }

            return $this->tokens[$i];
        }

        return null;
    }

    /**
     * Parse a single use statement.
     *
     * @return array A list with all found class names for a use statement.
     */
    public function parseUseStatement()
    {
        $class = '';
        $alias = '';
        $statements = array();
        $explicitAlias = false;
        while (($token = $this->next())) {
            $isNameToken = $token[0] === T_STRING || $token[0] === T_NS_SEPARATOR;
            if (!$explicitAlias && $isNameToken) {
                $class .= $token[1];
                $alias = $token[1];
            } else if ($explicitAlias && $isNameToken) {
                $alias .= $token[1];
            } else if ($token[0] === T_AS) {
                $explicitAlias = true;
                $alias = '';
            } else if ($token === ',') {
                $statements[strtolower($alias)] = $class;
                $class = '';
                $alias = '';
                $explicitAlias = false;
            } else if ($token === ';') {
                $statements[strtolower($alias)] = $class;
                break;
            } else {
                break;
            }
        }

        return $statements;
    }

    /**
     * Get all use statements.
     *
     * @param string $namespaceName The namespace name of the reflected class.
     * @return array A list with all found use statements.
     */
    public function parseUseStatements($namespaceName)
    {
        $statements = array();
        while (($token = $this->next())) {
            if ($token[0] === T_USE) {
                $statements = array_merge($statements, $this->parseUseStatement());
                continue;
            }
            if ($token[0] !== T_NAMESPACE || $this->parseNamespace() != $namespaceName) {
                continue;
            }

            // Get fresh array for new namespace. This is to prevent the parser to collect the use statements
            // for a previous namespace with the same name. This is the case if a namespace is defined twice
            // or if a namespace with the same name is commented out.
            $statements = array();
        }

        return $statements;
    }

    /**
     * Get the namespace.
     *
     * @return string The found namespace.
     */
    public function parseNamespace()
    {
        $name = '';
        while (($token = $this->next()) && ($token[0] === T_STRING || $token[0] === T_NS_SEPARATOR)) {
            $name .= $token[1];
        }

        return $name;
    }

    /**
     * Get the class name.
     *
     * @return string The foundclass name.
     */
    public function parseClass()
    {
        // Namespaces and class names are tokenized the same: T_STRINGs
        // separated by T_NS_SEPARATOR so we can use one function to provide
        // both.
        return $this->parseNamespace();
    }
}
