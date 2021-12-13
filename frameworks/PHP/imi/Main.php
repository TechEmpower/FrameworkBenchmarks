<?php
namespace ImiApp;

use Imi\App;
use Imi\Main\AppBaseMain;

class Main extends AppBaseMain
{
    public function __init()
    {
        // 这里可以做一些初始化操作，如果需要的话
        ini_set('memory_limit', -1);
        App::setDebug(true);
    }

}