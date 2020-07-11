<?php
namespace ImiApp\Listener;

use Imi\Db\Db;
use Imi\Event\EventParam;
use Imi\Redis\RedisManager;
use Imi\Event\IEventListener;
use Imi\Bean\Annotation\Listener;

/**
 * @Listener("IMI.APP.INIT")
 */
class AppInit implements IEventListener
{
    /**
     * 事件处理方法
     * @param EventParam $e
     * @return void
     */
    public function handle(EventParam $e)
    {
        $redis = RedisManager::getInstance();
        foreach(Db::query()->from('world')->select()->getArray() as $row)
        {
            $redis->set('world:' . $row['id'], $row);
        }
    }

}