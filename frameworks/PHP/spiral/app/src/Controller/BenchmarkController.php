<?php

declare(strict_types=1);

namespace App\Controller;

use App\Model\Fortune;
use App\Model\Repository\FortuneRepository;
use App\Model\Repository\WorldRepository;
use App\Model\World;
use Cycle\ORM\EntityManagerInterface;
use Nyholm\Psr7\Response;
use Psr\Http\Message\ResponseInterface;
use Spiral\Core\Container\SingletonInterface;
use Spiral\Views\ViewsInterface;

final class BenchmarkController implements SingletonInterface
{
    private ResponseInterface $plain;

    public function __construct(
        private readonly ViewsInterface $views,
        private readonly EntityManagerInterface $entityManager,
        private readonly FortuneRepository $fortunes,
        private readonly WorldRepository $worlds
    ) {
        $this->plain = new Response(200, ['Content-Type' => 'text/plain', 'Server' => 'Spiral']);
        $this->plain->getBody()->write('Hello, World!');
    }

    public function json(): array
    {
        return ['message' => 'Hello, World!'];
    }

    /**
     * @throws \Exception
     */
    public function db(): ?World
    {
        return $this->worlds->findRandom();
    }

    /**
     * @throws \Exception
     */
    public function queries(string|int $queries = 1): array
    {
        $queries = $this->clamp($queries);

        $worlds = [];
        while ($queries--) {
            $worlds[] = $this->worlds->findRandom();
        }

        return $worlds;
    }

    /**
     * @return mixed
     */
    public function fortunes(): string
    {
        $fortunes = (array)$this->fortunes->findAll();

        $fortune = new Fortune();
        $fortune->id = 0;
        $fortune->message = "Additional fortune added at request time.";

        $fortunes[] = $fortune;

        \usort($fortunes, static fn ($left, $right): int => \strcmp($left->message, $right->message));

        return $this->views->render('fortunes', ['fortunes' => $fortunes]);
    }

    /**
     * @throws \Throwable
     */
    public function updates(string|int $queries = 1): array
    {
        $queries = $this->clamp($queries);

        $worlds = [];
        while ($queries--) {
            $world = $this->worlds->findRandom();
            $world->randomNumber = \random_int(1, 10000);

            $this->entityManager->persist($world);

            $worlds[] = $world;
        }

        $this->entityManager->run();

        return $worlds;
    }

    public function plaintext(): ResponseInterface
    {
        return $this->plain;
    }

    private function clamp(string|int $value): int
    {
        if (!\is_numeric($value) || $value < 1) {
            return 1;
        }

        if ($value > 500) {
            return 500;
        }

        return (int)$value;
    }
}