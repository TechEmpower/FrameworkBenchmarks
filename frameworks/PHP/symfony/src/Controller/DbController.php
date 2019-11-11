<?php

namespace App\Controller;

use App\Repository\WorldRepository;
use Doctrine\ORM\EntityManagerInterface;
use Symfony\Component\HttpFoundation\JsonResponse;
use Symfony\Component\HttpFoundation\Request;
use Symfony\Component\Routing\Annotation\Route;

class DbController
{
    /** @var EntityManagerInterface */
    private $entityManager;

    /** @var WorldRepository */
    private $worldRepository;

    private function getUniqueRandomNumbers($count, $min, $max)
    {
        $res = [];
        do {
            $res[\mt_rand($min, $max)] = 1;
        } while (\count($res) < $count);

        return \array_keys($res);
    }

    public function __construct(EntityManagerInterface $entityManager, WorldRepository $worldRepository)
    {
        $this->entityManager = $entityManager;
        $this->worldRepository = $worldRepository;
    }

    /**
     * @Route("/db")
     */
    public function db(): JsonResponse
    {
        return new JsonResponse($this->worldRepository->find(\mt_rand(1, 10000)));
    }

    /**
     * @Route("/queries")
     */
    public function queries(Request $request): JsonResponse
    {
        $queries = (int) $request->query->get('queries', 1);
        $queries = \min(\max($queries, 1), 500);

        // possibility for enhancement is the use of SplFixedArray -> http://php.net/manual/de/class.splfixedarray.php
        $worlds = [];
        $numbers = $this->getUniqueRandomNumbers($queries, 1, 10000);
        foreach ($numbers as $id) {
            $worlds[] = $this->worldRepository->find($id);
        }

        return new JsonResponse($worlds);
    }

    /**
     * @Route("/updates")
     */
    public function update(Request $request): JsonResponse
    {
        $queries = (int) $request->query->get('queries', 1);
        $queries = \min(500, \max(1, $queries));

        $worlds = [];

        $numbers = $this->getUniqueRandomNumbers($queries, 1, 10000);
        foreach ($numbers as $id) {
            $world = $this->worldRepository->find($id);
            $world->setRandomNumber(\mt_rand(1, 10000));
            $worlds[] = $world;
            $this->entityManager->flush();
        }
        return new JsonResponse($worlds);
    }
}
