<?php
/**
 * Spiral Framework.
 *
 * @license   MIT
 * @author    Anton Titov (Wolfy-J)
 */
declare(strict_types=1);

namespace App\Controller;

use App\Model\Fortune;
use App\Model\Repository\FortuneRepository;
use App\Model\Repository\WorldRepository;
use App\Model\World;
use Cycle\ORM\Transaction;
use Nyholm\Psr7\Response;
use Spiral\Core\Container\SingletonInterface;
use Spiral\Views\ViewInterface;
use Spiral\Views\ViewsInterface;

final class BenchmarkController implements SingletonInterface
{
    /** @var ViewInterface */
    private $view;

    /** @var Transaction */
    private $transaction;

    /** @var FortuneRepository */
    private $fortunes;

    /** @var WorldRepository */
    private $worlds;

    /** @var Response */
    private $plain;

    /**
     * @param ViewsInterface    $views
     * @param Transaction       $transaction
     * @param FortuneRepository $fortunes
     * @param WorldRepository   $worlds
     */
    public function __construct(
        ViewsInterface $views,
        Transaction $transaction,
        FortuneRepository $fortunes,
        WorldRepository $worlds
    ) {
        $this->view = $views->get('fortunes');

        $this->transaction = $transaction;
        $this->fortunes = $fortunes;
        $this->worlds = $worlds;

        $this->plain = new Response(200, ['Content-Type' => 'text/plain', 'Server' => 'Spiral']);
        $this->plain->getBody()->write('Hello, World!');
    }

    /**
     * @return array
     */
    public function json()
    {
        return ['message' => 'Hello, World!'];
    }

    /**
     * @return World|null
     * @throws \Exception
     */
    public function db(): ?World
    {
        return $this->worlds->findRandom();
    }

    /**
     * @param int $queries
     * @return array
     * @throws \Exception
     */
    public function queries($queries = 1): array
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

        usort($fortunes, function ($left, $right) {
            return strcmp($left->message, $right->message);
        });

        return $this->view->render(compact('fortunes'));
    }

    /**
     * @param int $queries
     * @return array
     * @throws \Throwable
     */
    public function updates($queries = 1): array
    {
        $queries = $this->clamp($queries);

        $worlds = [];

        while ($queries--) {
            $world = $this->worlds->findRandom();
            $world->randomNumber = random_int(1, 10000);

            $this->transaction->persist($world)->run();

            $worlds[] = $world;
        }

        return $worlds;
    }

    /**
     * @return Response
     */
    public function plaintext(): Response
    {
        return $this->plain;
    }

    /**
     * @param $value
     * @return int
     */
    private function clamp($value): int
    {
        if (!is_numeric($value) || $value < 1) {
            return 1;
        }

        if ($value > 500) {
            return 500;
        }

        return (int)$value;
    }
}