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
namespace support\bootstrap\db;

use Webman\Bootstrap;
use support\Db;

/**
 * mysql心跳。定时发送一个查询，防止mysql连接长时间不活跃被mysql服务端断开。
 * 默认不开启，如需开启请到 config/bootstrap.php中添加 support\bootstrap\db\Heartbeat::class,
 * @package support\bootstrap\db
 */
class Heartbeat implements Bootstrap
{
    /**
     * @param \Workerman\Worker $worker
     *
     * @return void
     */
    public static function start($worker)
    {
        \Workerman\Timer::add(55, function (){
            Db::select('select 1 limit 1');
        });
    }
}
