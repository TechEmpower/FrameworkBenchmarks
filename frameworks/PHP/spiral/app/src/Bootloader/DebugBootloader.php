<?php
/**
 * Spiral Framework.
 *
 * @license   MIT
 * @author    Anton Titov (Wolfy-J)
 */
declare(strict_types=1);

namespace App\Bootloader;

use Psr\Http\Message\ResponseInterface;
use Psr\Http\Message\ServerRequestInterface;
use Psr\Http\Server\MiddlewareInterface;
use Psr\Http\Server\RequestHandlerInterface;
use Spiral\Boot\Bootloader\Bootloader;
use Spiral\Bootloader\Http\HttpBootloader;

class DebugBootloader extends Bootloader implements MiddlewareInterface
{
    public function boot(HttpBootloader $http)
    {
        $http->addMiddleware(self::class);
    }

    public function process(
        ServerRequestInterface $request,
        RequestHandlerInterface $handler
    ): ResponseInterface {
        try {
            return $handler->handle($request);
        } catch (\Throwable $e) {
            dumprr((string)$e);
            throw $e;
        }
    }
}