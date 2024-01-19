<?php
namespace ImiApp;

use Imi\App;
use Imi\Main\AppBaseMain;

class Main extends AppBaseMain
{
    public function __init(): void
    {
        // 这里可以做一些初始化操作，如果需要的话
        ini_set('memory_limit', -1);
        App::setDebug(false);
        if(extension_loaded('swoole'))
        {
            \Co::set([
                'socket_timeout'         => 30,
                'socket_connect_timeout' => 5,
                'socket_read_timeout'    => 30,
                'socket_write_timeout'   => 30,
            ]);
        }
    }

}