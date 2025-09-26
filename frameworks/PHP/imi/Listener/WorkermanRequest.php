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
 * @Listener(eventName="IMI.WORKERMAN.SERVER.HTTP.REQUEST", priority=20000000)
 */
class WorkermanRequest implements IEventListener
{
    /**
     * 事件处理方法.
     */
    public function handle(EventParam $e): void
    {
        $e->getData()['response']->setHeader('Date', App::get('test_date'));
    }
}
