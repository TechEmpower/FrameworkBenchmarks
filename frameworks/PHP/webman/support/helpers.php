<?php

/**
 * This file is part of webman.
 *
 * Licensed under The MIT License
 * For full copyright and license information, please see the MIT-LICENSE.txt
 * Redistributions of files must retain the above copyright notice.
 *
 * @author    walkor<walkor@workerman.net>
 * @copyright walkor<walkor@workerman.net>
 * @link      http://www.workerman.net/
 * @license   http://www.opensource.org/licenses/mit-license.php MIT License
 */

use support\Container;
use support\Request;
use support\Response;
use support\Translation;
use support\view\Raw;
use support\view\Blade;
use support\view\ThinkPHP;
use support\view\Twig;
use Workerman\Worker;
use Webman\App;
use Webman\Config;
use Webman\Route;

// Webman version
define('WEBMAN_VERSION', '1.4');

// Project base path
define('BASE_PATH', dirname(__DIR__));

/**
 * Generate paths based on given information
 * @param string $front
 * @param string $back
 * @return string
 */
function path_combine(string $front, string $back)
{
    return $front . ($back ? (DIRECTORY_SEPARATOR . ltrim($back, DIRECTORY_SEPARATOR)) : $back);
}

/**
 * return the program execute directory
 * @param string $path
 * @return string
 */
function run_path(string $path = '')
{
    static $run_path = '';
    if (!$run_path) {
        $run_path = \is_phar() ? \dirname(\Phar::running(false)) : BASE_PATH;
    }
    return \path_combine($run_path, $path);
}

/**
 * if the param $path equal false,will return this program current execute directory
 * @param string|false $path
 * @return false|string
 */
function base_path($path = '')
{
    if (false === $path) {
        return \run_path();
    }

    return \path_combine(BASE_PATH, $path);
}

/**
 * @param string $path
 * @return string
 */
function app_path(string $path = '')
{
    return \path_combine(BASE_PATH . DIRECTORY_SEPARATOR . 'app', $path);
}

/**
 * @param string $path
 * @return string
 */
function public_path(string $path = '')
{
    static $public_path = '';
    if (!$public_path) {
        $public_path = \config('app.public_path') ? : \run_path('public');
    }
    return \path_combine($public_path, $path);
}

/**
 * @param string $path
 * @return string
 */
function config_path(string $path = '')
{
    return \path_combine(BASE_PATH . DIRECTORY_SEPARATOR . 'config', $path);
}

/**
 * Phar support.
 * Compatible with the 'realpath' function in the phar file.
 * @param string $path
 * @return string
 */
function runtime_path(string $path = '')
{
    static $runtime_path = '';
    if (!$runtime_path) {
        $runtime_path = \config('app.runtime_path') ? : \run_path('runtime');
    }
    return \path_combine($runtime_path, $path);
}

/**
 * @param int $status
 * @param array $headers
 * @param string $body
 * @return Response
 */
function response($body = '', $status = 200, $headers = [])
{
    return new Response($status, $headers, $body);
}

/**
 * @param $data
 * @param int $options
 * @return Response
 */
function json($data, $options = JSON_UNESCAPED_UNICODE)
{
    return new Response(200, ['Content-Type' => 'application/json'], \json_encode($data, $options));
}

/**
 * @param $xml
 * @return Response
 */
function xml($xml)
{
    if ($xml instanceof SimpleXMLElement) {
        $xml = $xml->asXML();
    }
    return new Response(200, ['Content-Type' => 'text/xml'], $xml);
}

/**
 * @param $data
 * @param string $callback_name
 * @return Response
 */
function jsonp($data, $callback_name = 'callback')
{
    if (!\is_scalar($data) && null !== $data) {
        $data = \json_encode($data);
    }
    return new Response(200, [], "$callback_name($data)");
}

/**
 * @param string $location
 * @param int $status
 * @param array $headers
 * @return Response
 */
function redirect(string $location, int $status = 302, array $headers = [])
{
    $response = new Response($status, ['Location' => $location]);
    if (!empty($headers)) {
        $response->withHeaders($headers);
    }
    return $response;
}

/**
 * @param $template
 * @param array $vars
 * @param null $app
 * @return Response
 */
function view(string $template, array $vars = [], string $app = null)
{
    $request = \request();
    $plugin =  $request->plugin ?? '';
    $handler = \config($plugin ? "plugin.$plugin.view.handler" : 'view.handler');
    return new Response(200, [], $handler::render($template, $vars, $app));
}

/**
 * @param string $template
 * @param array $vars
 * @param string|null $app
 * @return Response
 * @throws Throwable
 */
function raw_view(string $template, array $vars = [], string $app = null)
{
    return new Response(200, [], Raw::render($template, $vars, $app));
}

/**
 * @param string $template
 * @param array $vars
 * @param string|null $app
 * @return Response
 */
function blade_view(string $template, array $vars = [], string $app = null)
{
    return new Response(200, [], Blade::render($template, $vars, $app));
}

/**
 * @param string $template
 * @param array $vars
 * @param string|null $app
 * @return Response
 */
function think_view(string $template, array $vars = [], string $app = null)
{
    return new Response(200, [], ThinkPHP::render($template, $vars, $app));
}

/**
 * @param string $template
 * @param array $vars
 * @param string|null $app
 * @return Response
 */
function twig_view(string $template, array $vars = [], string $app = null)
{
    return new Response(200, [], Twig::render($template, $vars, $app));
}

/**
 * @return \Webman\Http\Request|Request|null
 */
function request()
{
    return App::request();
}

/**
 * @param string|null $key
 * @param $default
 * @return array|mixed|null
 */
function config(string $key = null, $default = null)
{
    return Config::get($key, $default);
}

/**
 * @param string $name
 * @param ...$parameters
 * @return string
 */
function route(string $name, ...$parameters)
{
    $route = Route::getByName($name);
    if (!$route) {
        return '';
    }

    if (!$parameters) {
        return $route->url();
    }

    if (\is_array(\current($parameters))) {
        $parameters = \current($parameters);
    }

    return $route->url($parameters);
}

/**
 * @param mixed $key
 * @param mixed $default
 * @return mixed
 */
function session($key = null, $default = null)
{
    $session = \request()->session();
    if (null === $key) {
        return $session;
    }
    if (\is_array($key)) {
        $session->put($key);
        return null;
    }
    if (\strpos($key, '.')) {
        $key_array = \explode('.', $key);
        $value = $session->all();
        foreach ($key_array as $index) {
            if (!isset($value[$index])) {
                return $default;
            }
            $value = $value[$index];
        }
        return $value;
    }
    return $session->get($key, $default);
}

/**
 * @param string $id
 * @param array $parameters
 * @param string|null $domain
 * @param string|null $locale
 * @return string
 */
function trans(string $id, array $parameters = [], string $domain = null, string $locale = null)
{
    $res = Translation::trans($id, $parameters, $domain, $locale);
    return $res === '' ? $id : $res;
}

/**
 * @param null|string $locale
 * @return string
 */
function locale(string $locale = null)
{
    if (!$locale) {
        return Translation::getLocale();
    }
    Translation::setLocale($locale);
}

/**
 * 404 not found
 *
 * @return Response
 */
function not_found()
{
    return new Response(404, [], \file_get_contents(public_path() . '/404.html'));
}

/**
 * Copy dir.
 *
 * @param string $source
 * @param string $dest
 * @param bool $overwrite
 * @return void
 */
function copy_dir(string $source, string $dest, bool $overwrite = false)
{
    if (\is_dir($source)) {
        if (!is_dir($dest)) {
            \mkdir($dest);
        }
        $files = \scandir($source);
        foreach ($files as $file) {
            if ($file !== "." && $file !== "..") {
                \copy_dir("$source/$file", "$dest/$file");
            }
        }
    } else if (\file_exists($source) && ($overwrite || !\file_exists($dest))) {
        \copy($source, $dest);
    }
}

/**
 * Remove dir.
 *
 * @param string $dir
 * @return bool
 */
function remove_dir(string $dir)
{
    if (\is_link($dir) || \is_file($dir)) {
        return \unlink($dir);
    }
    $files = \array_diff(\scandir($dir), array('.', '..'));
    foreach ($files as $file) {
        (\is_dir("$dir/$file") && !\is_link($dir)) ? \remove_dir("$dir/$file") : \unlink("$dir/$file");
    }
    return \rmdir($dir);
}

/**
 * @param $worker
 * @param $class
 */
function worker_bind($worker, $class)
{
    $callback_map = [
        'onConnect',
        'onMessage',
        'onClose',
        'onError',
        'onBufferFull',
        'onBufferDrain',
        'onWorkerStop',
        'onWebSocketConnect'
    ];
    foreach ($callback_map as $name) {
        if (\method_exists($class, $name)) {
            $worker->$name = [$class, $name];
        }
    }
    if (\method_exists($class, 'onWorkerStart')) {
        \call_user_func([$class, 'onWorkerStart'], $worker);
    }
}

/**
 * @param $process_name
 * @param $config
 * @return void
 */
function worker_start($process_name, $config)
{
    $worker = new Worker($config['listen'] ?? null, $config['context'] ?? []);
    $property_map = [
        'count',
        'user',
        'group',
        'reloadable',
        'reusePort',
        'transport',
        'protocol',
    ];
    $worker->name = $process_name;
    foreach ($property_map as $property) {
        if (isset($config[$property])) {
            $worker->$property = $config[$property];
        }
    }

    $worker->onWorkerStart = function ($worker) use ($config) {
        require_once \base_path() . '/support/bootstrap.php';

        foreach ($config['services'] ?? [] as $server) {
            if (!\class_exists($server['handler'])) {
                echo "process error: class {$server['handler']} not exists\r\n";
                continue;
            }
            $listen = new Worker($server['listen'] ?? null, $server['context'] ?? []);
            if (isset($server['listen'])) {
                echo "listen: {$server['listen']}\n";
            }
            $instance = Container::make($server['handler'], $server['constructor'] ?? []);
            \worker_bind($listen, $instance);
            $listen->listen();
        }

        if (isset($config['handler'])) {
            if (!\class_exists($config['handler'])) {
                echo "process error: class {$config['handler']} not exists\r\n";
                return;
            }

            $instance = Container::make($config['handler'], $config['constructor'] ?? []);
            \worker_bind($worker, $instance);
        }
    };
}

/**
 * Phar support.
 * Compatible with the 'realpath' function in the phar file.
 *
 * @param string $file_path
 * @return string
 */
function get_realpath(string $file_path): string
{
    if (\strpos($file_path, 'phar://') === 0) {
        return $file_path;
    } else {
        return \realpath($file_path);
    }
}

/**
 * @return bool
 */
function is_phar()
{
    return \class_exists(\Phar::class, false) && Phar::running();
}

/**
 * @return int
 */
function cpu_count()
{
    // Windows does not support the number of processes setting.
    if (\DIRECTORY_SEPARATOR === '\\') {
        return 1;
    }
    $count = 4;
    if (\is_callable('shell_exec')) {
        if (\strtolower(PHP_OS) === 'darwin') {
            $count = (int)\shell_exec('sysctl -n machdep.cpu.core_count');
        } else {
            $count = (int)\shell_exec('nproc');
        }
    }
    return $count > 0 ? $count : 4;
}
