<?php

use Swow\Channel;

class Pool
{

    protected static Channel $channel;

    public function __construct($dsn, $username, $password, $size)
    {
        static::$channel = new Channel($size);
        for ($i = 0; $i < $size; $i++) {
            static::$channel->push(new PDO($dsn, $username, $password,[
                PDO::ATTR_DEFAULT_FETCH_MODE => PDO::FETCH_ASSOC
            ]));
        }
    }

    public function get(): PDO
    {
        return static::$channel->pop();
    }

    public function put(PDO $pdo)
    {
        return static::$channel->push($pdo);
    }
}