<?php

/*
 * This file is part of the Symfony package.
 *
 * (c) Fabien Potencier <fabien@symfony.com>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

namespace Symfony\Component\Yaml\Exception;

/**
 * Exception class thrown when an error occurs during parsing.
 *
 * @author Fabien Potencier <fabien@symfony.com>
 *
 * @api
 */
class ParseException extends \RuntimeException implements ExceptionInterface
{
    private $parsedFile;
    private $parsedLine;
    private $snippet;
    private $rawMessage;

    /**
     * Constructor.
     *
     * @param string    $message    The error message
     * @param integer   $parsedLine The line where the error occurred
     * @param integer   $snippet    The snippet of code near the problem
     * @param string    $parsedFile The file name where the error occurred
     * @param Exception $previous   The previous exception
     */
    public function __construct($message, $parsedLine = -1, $snippet = null, $parsedFile = null, Exception $previous = null)
    {
        $this->parsedFile = $parsedFile;
        $this->parsedLine = $parsedLine;
        $this->snippet = $snippet;
        $this->rawMessage = $message;

        $this->updateRepr();

        parent::__construct($this->message, 0, $previous);
    }

    /**
     * Gets the snippet of code near the error.
     *
     * @return string The snippet of code
     */
    public function getSnippet()
    {
        return $this->snippet;
    }

    /**
     * Sets the snippet of code near the error.
     *
     * @param string $snippet The code snippet
     */
    public function setSnippet($snippet)
    {
        $this->snippet = $snippet;

        $this->updateRepr();
    }

    /**
     * Gets the filename where the error occurred.
     *
     * This method returns null if a string is parsed.
     *
     * @return string The filename
     */
    public function getParsedFile()
    {
        return $this->parsedFile;
    }

    /**
     * Sets the filename where the error occurred.
     *
     * @param string $parsedFile The filename
     */
    public function setParsedFile($parsedFile)
    {
        $this->parsedFile = $parsedFile;

        $this->updateRepr();
    }

    /**
     * Gets the line where the error occurred.
     *
     * @return integer The file line
     */
    public function getParsedLine()
    {
        return $this->parsedLine;
    }

    /**
     * Sets the line where the error occurred.
     *
     * @param integer $parsedLine The file line
     */
    public function setParsedLine($parsedLine)
    {
        $this->parsedLine = $parsedLine;

        $this->updateRepr();
    }

    private function updateRepr()
    {
        $this->message = $this->rawMessage;

        $dot = false;
        if ('.' === substr($this->message, -1)) {
            $this->message = substr($this->message, 0, -1);
            $dot = true;
        }

        if (null !== $this->parsedFile) {
            $this->message .= sprintf(' in %s', json_encode($this->parsedFile));
        }

        if ($this->parsedLine >= 0) {
            $this->message .= sprintf(' at line %d', $this->parsedLine);
        }

        if ($this->snippet) {
            $this->message .= sprintf(' (near "%s")', $this->snippet);
        }

        if ($dot) {
            $this->message .= '.';
        }
    }
}
