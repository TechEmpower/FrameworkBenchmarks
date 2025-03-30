<?php

declare(strict_types=1);

namespace Cyber;

use Exception;

class Response
{
    /**
     * HTTP status codes and their descriptions
     */
    private const HTTP_STATUS = [
        // 2xx Success
        200 => 'OK',
        201 => 'Created',
        204 => 'No Content',

        // 3xx Redirection
        301 => 'Moved Permanently',
        302 => 'Found',
        304 => 'Not Modified',

        // 4xx Client Errors
        400 => 'Bad Request',
        401 => 'Unauthorized',
        403 => 'Forbidden',
        404 => 'Not Found',
        405 => 'Method Not Allowed',
        408 => 'Request Timeout',
        422 => 'Unprocessable Entity',
        429 => 'Too Many Requests',

        // 5xx Server Errors
        500 => 'Internal Server Error',
        502 => 'Bad Gateway',
        503 => 'Service Unavailable',
        504 => 'Gateway Timeout'
    ];

    protected string $content = '';
    protected int $statusCode = 200;
    protected array $headers = [];
    protected bool $sent = false;

    public function __construct(string $content = '', int $statusCode = 200, array $headers = [])
    {
        $this->content = $content;
        $this->statusCode = $statusCode;
        $this->headers = $headers;
    }

    /**
     * Get status code description
     */
    public function getStatusText(): string
    {
        return self::HTTP_STATUS[$this->statusCode];
    }
    /**
     * Get response body
     */
    public function getStatusCode(): int
    {
        return $this->statusCode;
    }
    /**
     * Get response body
     */
    public function getContent(): string
    {
        return $this->content;
    }

    /**
     * Get all response headers
     */
    public function getHeaders(): array
    {
        return $this->headers;
    }

    /**
     * Add response header
     *
     * @throws Exception
     */
    public function withHeader(string $name, string|array $value): static
    {
        // Validate header name legality
        if (!preg_match('/^[a-zA-Z0-9\'`#$%&*+.^_|~!-]+$/', $name)) {
            throw new Exception('Header name can only contain letters, numbers, and special characters');
        }
        if (empty($name)) {
            throw new Exception('Header name cannot be empty');
        }

        $clone = clone $this;
        $clone->headers[$name] = is_array($value) ? $value : [$value];
        return $clone;
    }

    /**
     * Add multiple response headers
     */
    public function withHeaders(array $headers): static
    {
        $clone = clone $this;
        foreach ($headers as $name => $value) {
            $clone = $clone->withHeader($name, $value);
        }
        return $clone;
    }

    /**
     * Create JSON response
     *
     * @throws Exception
     */
    public static function json(mixed $data, int $status = 200, array $headers = []): static
    {
        try {
            $json = json_encode($data, JSON_THROW_ON_ERROR | JSON_UNESCAPED_UNICODE);
        } catch (\JsonException $e) {
            throw new Exception('Unable to encode data to JSON', 0, $e);
        }

        $headers['Content-Type'] = 'application/json; charset=utf-8';
        $headers['Date'] = gmdate(DATE_RFC7231);
        return new static($json, $status, $headers);
    }

    /**
     * Create HTML response
     */
    public static function html(string $html, int $status = 200, array $headers = []): static
    {
        $headers['Content-Type'] = 'text/html; charset=utf-8';
        $headers['Date'] = gmdate(DATE_RFC7231);
        return new static($html, $status, $headers);
    }

    /**
     * Create text response
     */
    public static function text(string $text, int $status = 200, array $headers = []): static
    {
        $headers['Content-Type'] = 'text/plain; charset=utf-8';
        $headers['Date'] = gmdate(DATE_RFC7231);
        return new static($text, $status, $headers);
    }
    /**
     * Create file response
     */
    public static function file(string $file, string $filename, int $status = 200, array $headers = []): static
    {
        $headers['Content-Type'] = 'application/octet-stream';
        $headers['Date'] = gmdate(DATE_RFC7231);
        $headers['Content-Disposition'] = 'attachment; filename="' . $filename . '"';
        return new static(file_get_contents($file), $status, $headers);
    }

    /**
     * Create redirect response
     *
     * @throws Exception
     */
    public static function redirect(string $url, int $status = 302, array $headers = []): static
    {
        if (!filter_var($url, FILTER_VALIDATE_URL) && !str_starts_with($url, '/')) {
            throw new Exception('Invalid URL format');
        }
        return new static('', $status, array_merge($headers, ['Location' => $url]));
    }

    /**
     * Send response
     *
     * @throws Exception
     */
    public function send(): void
    {
        if ($this->isSent()) {
            throw new Exception('Response already sent');
        }
        if (!headers_sent()) {
        //     // 发送状态码
            http_response_code($this->statusCode);

        //     // 确保有 Content-Type 头
            if (!isset($this->headers['Content-Type'])) {
                $this->headers['Content-Type'] = ['text/html; charset=utf-8'];
            }

        //     // 发送响应头
            foreach ($this->headers as $name => $values) {
                $values = (array) $values;
                foreach ($values as $value) {
                    header($name . ': ' . $value, false);
                }
            }
        }

        // // 发送响应内容
        echo $this->content;

        // $this->sent = true;
    }

    /**
     * Check if the response has been sent
     */
    public function isSent(): bool
    {
        return $this->sent;
    }
}
