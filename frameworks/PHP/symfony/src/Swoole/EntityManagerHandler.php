<?php

namespace App\Swoole;

use Doctrine\ORM\EntityManagerInterface;
use K911\Swoole\Server\RequestHandler\RequestHandlerInterface;
use Swoole\Http\Request;
use Swoole\Http\Response;

final class EntityManagerHandler implements RequestHandlerInterface
{
    private $decorated;
    private $entityManager;

    public function __construct(RequestHandlerInterface $decorated, EntityManagerInterface $entityManager)
    {
        $this->decorated = $decorated;
        $this->entityManager = $entityManager;
    }

    /**
     * {@inheritdoc}
     */
    public function handle(Request $request, Response $response): void
    {
        try {
            $this->decorated->handle($request, $response);
        } finally {
            // Swoole handle several request in a raw. We clear the entityManager between 2 call, to avoid Doctrine
            // to re-use the same objects without fetching it from the database.
            $this->entityManager->clear();
        }
    }
}
