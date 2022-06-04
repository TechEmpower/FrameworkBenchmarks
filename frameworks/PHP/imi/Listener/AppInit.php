<?php

declare(strict_types=1);

namespace ImiApp\Listener;

use Imi\App;
use Imi\Db\Db;
use Imi\Timer\Timer;
use Imi\Event\EventParam;
use Imi\Queue\Model\Message;
use Imi\Event\IEventListener;
use Imi\Aop\Annotation\Inject;
use Imi\Bean\Annotation\Listener;

/**
 * @Listener("IMI.MAIN_SERVER.WORKER.START.APP")
 * @Listener("IMI.WORKERMAN.SERVER.WORKER_START")
 */
class AppInit implements IEventListener
{
    /**
     * 事件处理方法.
     */
    public function handle(EventParam $e): void
    {
        App::set('worlds', Db::query()->from('world')->select()->getArray());
    }
}
