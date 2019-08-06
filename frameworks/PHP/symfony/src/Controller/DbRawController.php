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
     * @throws
     */
    public function db(): JsonResponse
    {
        $statement = $this->connection->prepare('SELECT * FROM world WHERE id = ?');
        $statement->execute([mt_rand(1, 10000)]);
        $world = $statement->fetch(FetchMode::ASSOCIATIVE);

        return new JsonResponse($world);
    }

    /**
     * @Route("/raw/queries")
     * @throws
     */
    public function queries(Request $request): JsonResponse
    {
        $queries = $request->query->getInt('queries', 1);
        $queries = min(max($queries, 1), 500);

        // possibility for enhancement is the use of SplFixedArray -> http://php.net/manual/de/class.splfixedarray.php
        $worlds = [];

        $statement = $this->connection->prepare('SELECT * FROM world WHERE id = ?');
        for ($i = 0; $i < $queries; ++$i) {
            $statement->execute([mt_rand(1, 10000)]);
            $worlds[] = $statement->fetch(FetchMode::ASSOCIATIVE);
        }

        return new JsonResponse($worlds);
    }

    /**
     * @Route("/raw/updates")
     * @throws
     */
    public function updates(Request $request): JsonResponse
    {
        $queries = $request->query->getInt('queries', 1);
        $queries = min(500, max(1, $queries));

        $worlds = [];

        $statement = $this->connection->prepare('UPDATE world SET randomNumber=? WHERE id=?');
        for ($i = 0; $i < $queries; ++$i) {
            $id = mt_rand(1, 10000);
            $randomNumber = mt_rand(1, 10000);
            $statement->execute([$randomNumber, $id]);
            $worlds[] = ['id' => $id, 'randomNumber' => $randomNumber];
        }

        return new JsonResponse($worlds);
    }
}
