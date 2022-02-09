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
        $world = $statement->execute([mt_rand(1, 10000)]);

        return new JsonResponse($world->fetchAssociative());
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
            $world = $statement->execute([mt_rand(1, 10000)]);
            $worlds[] = $world->fetchAssociative();
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
            $world = $readStatement->execute([$id]);
            $world =  $world->fetchAssociative();
            $writeStatement->execute(
                [$world['randomNumber'] = mt_rand(1, 10000), $id]
            );
            $worlds[] = $world;
        }

        return new JsonResponse($worlds);
    }
}
