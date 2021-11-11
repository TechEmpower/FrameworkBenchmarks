<?php

namespace App\Swoole;

use K911\Swoole\Server\WorkerHandler\WorkerStartHandlerInterface;
use Swoole\Http\Request;
use Swoole\Http\Response;
use Swoole\Server;

final class SrandStartHandler implements WorkerStartHandlerInterface
{
    private $decorated;

    public function __construct(?WorkerStartHandlerInterface $decorated)
    {
        $this->decorated = $decorated;
    }

    public function handle(Server $worker, int $workerId): void
    {
        if ($this->decorated) {
            $this->decorated->handle($worker, $workerId);
        }

        // Seed the random generator
        \mt_srand();
    }
}
