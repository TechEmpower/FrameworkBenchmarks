<?php

namespace App\Controller;

use Doctrine\DBAL\Connection;
use Doctrine\DBAL\FetchMode;
use Symfony\Component\HttpFoundation\JsonResponse;
use Symfony\Component\HttpFoundation\Request;
use Symfony\Component\Routing\Annotation\Route;

class DbRawController
{
    /** @var Connection */
    private $connection;

    public function __construct(Connection $connection)
    {
        $this->connection = $connection;
    }

    /**
     * @Route("/raw/db")
     */
    public function db(): JsonResponse
    {
        $statement = $this->connection->prepare('SELECT id,randomNumber FROM World WHERE id = ?');
        $statement->execute([mt_rand(1, 10000)]);
        $world = $statement->fetch(FetchMode::ASSOCIATIVE);

        return new JsonResponse($world);
    }

    /**
     * @Route("/raw/queries")
     */
    public function queries(Request $request): JsonResponse
    {
        $queries = (int) $request->query->get('queries', 1);
        $queries = min(max($queries, 1), 500);

        // possibility for enhancement is the use of SplFixedArray -> http://php.net/manual/de/class.splfixedarray.php
        $worlds = [];

        $statement = $this->connection->prepare('SELECT id,randomNumber FROM World WHERE id = ?');
        for ($i = 0; $i < $queries; ++$i) {
            $statement->execute([mt_rand(1, 10000)]);
            $worlds[] = $statement->fetch(FetchMode::ASSOCIATIVE);
        }

        return new JsonResponse($worlds);
    }

    /**
     * @Route("/raw/updates")
     */
    public function updates(Request $request): JsonResponse
    {
        $queries = (int) $request->query->get('queries', 1);
        $queries = min(500, max(1, $queries));

        $worlds = [];

        $writeStatement = $this->connection->prepare('UPDATE World SET randomNumber= ? WHERE id= ?');
        $readStatement = $this->connection->prepare('SELECT id,randomNumber FROM World WHERE id = ?');

        for ($i = 0; $i < $queries; ++$i) {
            $id = mt_rand(1, 10000);
            $readStatement->execute([$id]);
            $world =  $readStatement->fetch(FetchMode::ASSOCIATIVE);
            $writeStatement->execute(
                [$world['randomNumber'] = mt_rand(1, 10000), $id]
            );
            $worlds[] = $world;
        }

        return new JsonResponse($worlds);
    }
}
