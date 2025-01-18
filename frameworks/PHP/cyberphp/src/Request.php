<?php

declare(strict_types=1);

namespace Cyber;

/**
* HTTP Request Class
*
* This class encapsulates HTTP request information and provides a standard interface for accessing request data.
* The design references Laravel's Request class and implements common request handling functions.
*/
class Request
{
    public static function getPathInfo(): string
    {
        $requestUri = $_SERVER['REQUEST_URI'] ?? '/';

        // Remove query string
        if (($pos = strpos($requestUri, '?')) !== false) {
            $requestUri = substr($requestUri, 0, $pos);
        }

        // Remove script name (if any)
        $scriptName = $_SERVER['SCRIPT_NAME'] ?? '';
        if ($scriptName && strpos($requestUri, $scriptName) === 0) {
            $requestUri = substr($requestUri, strlen($scriptName));
        }
        return $requestUri ?: '/';
    }
    /**
    * Get application name
    *
    * @return string|null
    */
    public function getAppName(): ?string
    {
        // Get the current request URI
        $uri = static::getPathInfo();
        $segments = explode('/', trim($uri, '/')); // Split the URI by slashes
        // Return application name
        return isset($segments[0]) ? $segments[0] : '';
    }
    /**
    * Get request method
    *
    * @return string
    */
    public function getMethod(): string
    {
        return strtoupper($_SERVER['REQUEST_METHOD'] ?? 'GET');
    }

    /**
    * Get GET parameter
    *
    * @param string|null $key Parameter name
    * @param mixed $default Default value
    * @return mixed
    */
    public function query(?string $key = null, $default = null)
    {
        if ($key === null) {
            return $_GET;
        }
        return $_GET[$key] ?? $default;
    }

    /**
    * Get POST parameter
    *
    * @param string|null $key Parameter name
    * @param mixed $default Default value
    * @return mixed
    */
    public function post(?string $key = null, $default = null)
    {
        if ($key === null) {
            return $_POST;
        }
        return $_POST[$key] ?? $default;
    }

    /**
    * Get all input parameters (GET + POST)
    *
    * @param string|null $key Parameter name
    * @param mixed $default Default value
    * @return mixed
    */
    public function all(?string $key = null, $default = null)
    {
        $data = array_merge($_GET, $_POST, self::getParsedContent());

        if ($key === null) {
            return $data;
        }
        return $data[$key] ?? $default;
    }

    /**
    * Get parsed request content
    *
    * @return array
    */
    protected function getParsedContent(): array
    {
        $parsedContent = file_get_contents('php://input');

        if (self::isJson()) {
            $content = json_decode($parsedContent, true);
            if (json_last_error() === JSON_ERROR_NONE) {
                $parsedContent = $content;
            }
        }
        return $parsedContent;
    }

    /**
    * Get Cookie value
    *
    * @param string|null $key Cookie name
    * @param mixed $default Default value
    * @return mixed
    */
    public function cookie(?string $key = null, $default = null)
    {
        if ($key === null) {
            return $_COOKIE;
        }
        return $_COOKIE[$key] ?? $default;
    }

    /**
    * Check if it is an AJAX request
    *
    * @return bool
    */
    public function isAjax(): bool
    {
        return isset($_SERVER['HTTP_X_REQUESTED_WITH']) && $_SERVER['HTTP_X_REQUESTED_WITH'] === 'XMLHttpRequest';
    }

    /**
    * Check if it is a JSON request
    *
    * @return bool
    */
    public function isJson(): bool
    {
        $contentType = $_SERVER['CONTENT_TYPE'] ?? '';
        return str_contains($contentType, '/json') || str_contains($contentType, '+json');
    }
    /**
    * Check if it is a JSON request
    *
    * @return bool
    */
    public function rawBody(): bool
    {
        return $_SERVER['RAW_BODY'] ?? file_get_contents("php://input");
    }

    /**
    * Get client IP address
    *
    * @return string
    */
    public function getClientIp(): string
    {
        $keys = ['HTTP_X_FORWARDED_FOR', 'HTTP_CLIENT_IP', 'REMOTE_ADDR'];

        foreach ($keys as $key) {
            if (isset($_SERVER[$key])) {
                return $_SERVER[$key];
            }
        }

        return '';
    }

    /**
    * Get the complete URL of the request
    *
    * @return string
    */
    public function getUrl(): string
    {
        $host = $_SERVER['HTTP_HOST'] ?? $_SERVER['SERVER_NAME'];
        $uri = $_SERVER['REQUEST_URI'] ?? '';
        return "{$host}{$uri}";
    }

    /**
    * Get specified request header
    *
    * @param string $key Request header name
    * @param mixed $default Default value
    * @return mixed
    */
    public function header($key=null, $default = null)
    {
        if($key === null) {
            $headers = [];
            foreach ($_SERVER as $key => $value) {
                if (strpos($key, 'HTTP_') === 0) {
                    $key = str_replace(' ', '-', ucwords(strtolower(str_replace('_', ' ', substr($key, 5)))));
                    $headers[$key] = $value;
                }
            }
            ksort($headers);
            return $headers;
        }
        return $_SERVER['HTTP_'.strtoupper(str_replace('-', '_', $key))] ?? $default;
    }
    /**
    * Set request header
    * @param string $key Header information name
    * @param string $value Header information value
    * @return void
    */
    public function setHeader(string $key, string $value): void
    {
        if(is_array($key)){
            foreach ($key as $k => $v) {
                $_SERVER['HTTP_' . strtoupper(str_replace('-', '_', $k))] = $v;
            }
        }else{
            $_SERVER['HTTP_' . strtoupper(str_replace('-', '_', $key))] = $value;
        }
    }
}
