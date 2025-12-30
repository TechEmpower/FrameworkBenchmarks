<?php
namespace ImiApp\Listener;

use Imi\App;
use Imi\Db\Db;
use Imi\Timer\Timer;
use Imi\Event\EventParam;
use Imi\Redis\RedisManager;
use Imi\Event\IEventListener;
use Imi\Bean\Annotation\Listener;

/**
 * @Listener("IMI.WORKERMAN.SERVER.WORKER_START")
 */
class WorkermanWorkerStart implements IEventListener
{
    /**
     * 事件处理方法.
     */
    public function handle(EventParam $e): void
    {
        App::set('test_date', gmdate(DATE_RFC7231));
        Timer::tick(1000, function() {
            App::set('test_date', gmdate(DATE_RFC7231));
        });
    }

}
