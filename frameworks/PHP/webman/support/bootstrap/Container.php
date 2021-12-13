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

use Workerman\Worker;
use Webman\Bootstrap;
use Psr\Container\ContainerInterface;

/**
 * Class Container
 * @package support
 * @method static mixed get($name)
 * @method static mixed make($name, array $parameters)
 * @method static bool has($name)
 */
class Container implements Bootstrap
{
    /**
     * @var ContainerInterface
     */
    protected static $_instance = null;

    /**
     * @param Worker $worker
     * @return void
     */
    public static function start($worker)
    {
        static::$_instance = include config_path() . '/container.php';
    }

    /**
     * @param $name
     * @param $arguments
     * @return mixed
     */
    public static function __callStatic($name, $arguments)
    {
        return static::$_instance->{$name}(... $arguments);
    }

    /**
     * instance
     * @return
     */
    public static function instance()
    {
        return static::$_instance;
    }
}