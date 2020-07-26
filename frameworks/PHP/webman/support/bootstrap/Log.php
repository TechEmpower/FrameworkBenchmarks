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
namespace support\bootstrap;

use Webman\Bootstrap;
use Monolog\Logger;

/**
 * Class Redis
 * @package support
 *
 * @method static void log($level, $message, array $context = [])
 * @method static void debug($message, array $context = [])
 * @method static void info($message, array $context = [])
 * @method static void notice($message, array $context = [])
 * @method static void warning($message, array $context = [])
 * @method static void error($message, array $context = [])
 * @method static void critical($message, array $context = [])
 * @method static void alert($message, array $context = [])
 * @method static void emergency($message, array $context = [])
 */
class Log implements Bootstrap {

    /**
     * @var array
     */
    protected static $_instance = [];

    /**
     * @param \Workerman\Worker $worker
     * @return void
     */
    public static function start($worker)
    {
        $configs = config('log', []);
        foreach ($configs as $channel => $config) {
            $logger = static::$_instance[$channel] = new Logger($channel);
            foreach ($config['handlers'] as $handler_config) {
                $handler = new $handler_config['class'](... \array_values($handler_config['constructor']));
                if (isset($handler_config['formatter'])) {
                    $formatter = new $handler_config['formatter']['class'](... \array_values($handler_config['formatter']['constructor']));
                    $handler->setFormatter($formatter);
                }
                $logger->pushHandler($handler);
            }
        }
    }

    /**
     * @param string $name
     * @return Logger;
     */
    public static function channel($name = 'default')
    {
        return static::$_instance[$name] ?? null;
    }


    /**
     * @param $name
     * @param $arguments
     * @return mixed
     */
    public static function __callStatic($name, $arguments)
    {
        return static::channel('default')->{$name}(... $arguments);
    }
}