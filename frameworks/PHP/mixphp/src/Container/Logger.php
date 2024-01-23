<?php

namespace App\Container;

use Monolog\Handler\HandlerInterface;
use Monolog\Handler\RotatingFileHandler;

/**
 * Class Logger
 * @package App\Container
 */
class Logger implements HandlerInterface
{

    static private $instance;

    /**
     * @return \Monolog\Logger
     */
    public static function instance(): \Monolog\Logger
    {
        if (!isset(self::$instance)) {
            $logger = new \Monolog\Logger('MIX');
            $logger->pushHandler(new RotatingFileHandler(__DIR__ . '/../../runtime/logs/mix.log', 7));
            $logger->pushHandler(new Logger());
            self::$instance = $logger;
        }
        return self::$instance;
    }

    public function isHandling(array $record): bool
    {
        return $record['level'] >= \Monolog\Logger::DEBUG;
    }

    public function handle(array $record): bool
    {
        printf("%s  %s  %s\n", $record['datetime']->format('Y-m-d H:i:s.u'), $record['level_name'], $record['message']);
        return false;
    }

    public function handleBatch(array $records): void
    {
        // TODO: Implement handleBatch() method.
    }

    public function close(): void
    {
        // TODO: Implement close() method.
    }

}
