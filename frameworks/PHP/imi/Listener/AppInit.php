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
        if(getenv('WITH_REDIS') ?? false)
        {
            $redis = RedisManager::getInstance();
            $page = 1;
            while($list = Db::query()->from('world')->page($page, 1000)->select()->getArray())
            {
                $redisList = [];
                foreach($list as $row)
                {
                    $redisList['world:' . $row['id']] = $row;
                }
                $redis->mset($redisList);
                ++$page;
            }
        }
    }

}