<?php
declare(strict_types=1);

/**
* Get the application instance
* If there is a configuration file, create the application instance
* If there is no configuration file, return the already created instance
* If there is no configuration file and no instance has been created, throw an exception
* @param string $bootstrap Configuration file
* @return \Cyber\App
*/
if (!function_exists('app')) {
    function app(): \Cyber\App
    {
        return \Cyber\App::getInstance();
    }
}

/**
* Get configuration information
* config();                  // Returns all configurations
* config('app.name');        // Gets the value of app.name
* config('db.mysql.host');   // Gets the value of db.mysql.host
* @param string|null $key Configuration key name, supports multi-level configuration separated by dots, e.g., 'app.debug'
* @param mixed $default Default value, returned when the configuration item does not exist
* @return mixed
*/
if (!function_exists('config')) {
    function config(?string $key = null, $default = null)
    {
        return \Cyber\App::getInstance()->getConfig($key) ?? $default;
    }
}

function renderExceptionPage($e, $debug = true, $templateFile = ''): string
{
    // Determine template path
    $templateFile = !empty($templateFile) ? $templateFile : __DIR__ . '/views/errors/exception.html';
    // Prepare template variables
    $data = [
        'code' => $e->getCode(),
        'message' => $debug ? $e->getMessage() : 'The current server is experiencing an error, please contact the administrator or try again later.',
        'error' => $e->getMessage(),
    ];
    // Add more information in debug mode
    if ($debug) {
        $data['trace'] = [];
        $data['file'] = $e->getFile();
        $data['line'] = $e->getLine();
        $traceFiles = $e->getTrace();
        array_unshift($traceFiles, ['file' => $data['file'], 'line' => $data['line']]);
        foreach ($traceFiles as $v) {
            try {
                if (isset($v['file']) && isset($v['line'])) {
                    $startline = max(1, $v['line'] - 10);
                    $contents  = file($v['file']);
                    $data['trace'][] = [
                        'file' => $v['file'],
                        'line' => $v['line'],
                        'source0' => $contents ? array_slice($contents, 0, 1) : '',
                        'source' => [
                            'startline' => $startline,
                            'content' => array_slice($contents, $startline - 1, 16)
                        ]
                    ];
                }
            } catch (\Throwable $e) {
                continue;
            }
        }
    }
    // Render error page
    if (!file_exists($templateFile)) {
        $msg = '<div style="margin:100px auto 20px;text-align:center"><h1>Error ' . $data['code'] . '</h1></div>';
        $msg .= '<div style="margin:0px auto;text-align:center">Sorry, the server encountered an error</div>';
        $msg .= '<div style="margin:50px auto;text-align:center"><h2><pre>' . htmlspecialchars($data['message']) . '</pre></h2></div>';
        $msg .= '<div style="margin:100px auto;text-align:center"><a href="/"><button>Return to Home</button></a></div>';
        return $msg;
    }
    extract($data);
    ob_start();
    include $templateFile;
    return ob_get_clean();
}