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
use Workerman\Timer;

/**
 * Class Date
 * @package support\bootstrap
 */
class Date implements Bootstrap {

    public static $date = '';

    /**
     * @param \Workerman\Worker $worker
     * @return void
     */
    public static function start($worker)
    {
        self::$date = gmdate('D, d M Y H:i:s').' GMT';
        Timer::add(1, function() {
            self::$date = gmdate('D, d M Y H:i:s').' GMT';
        });
    }

}