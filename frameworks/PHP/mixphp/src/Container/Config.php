<?php

namespace App\Container;

/**
 * Class Config
 * @package App\Container
 */
class Config
{

    /**
     * @var \Noodlehaus\Config
     */
    static private $instance;

    /**
     * @return \Noodlehaus\Config
     */
    public static function instance(): \Noodlehaus\Config
    {
        if (!isset(self::$instance)) {
            self::$instance = new \Noodlehaus\Config(__DIR__ . '/../../conf');
        }
        return self::$instance;
    }

}