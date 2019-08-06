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

    public function __construct(EntityManagerInterface $entityManager, WorldRepository $worldRepository)
    {
        $this->entityManager = $entityManager;
        $this->worldRepository = $worldRepository;
    }

    /**
     * @Route("/db")
     * @throws
     */
    public function db(): JsonResponse
    {
        $world = $this->worldRepository->find(mt_rand(1, 10000));

        return new JsonResponse($world);
    }
    /**
     * @Route("/queries")
     * @throws
     */
    public function queries(Request $request): JsonResponse
    {
        $queries = $request->query->getInt('queries', 1);
        $queries = min(max($queries, 1), 500);

        // possibility for enhancement is the use of SplFixedArray -> http://php.net/manual/de/class.splfixedarray.php
        $worlds = [];

        for ($i = 0; $i < $queries; ++$i) {
            $worlds[] = $this->worldRepository->find(mt_rand(1, 10000));
        }

        return new JsonResponse($worlds);
    }

    /**
     * @Route("/updates")
     * @throws
     */
    public function update(Request $request): JsonResponse
    {
        $queries = $request->query->getInt('queries', 1);
        $queries = min(500, max(1, $queries));

        $worlds = [];

        for ($i = 0; $i < $queries; ++$i) {
            $world = $this->worldRepository->find(mt_rand(1, 10000));
            if ($world) {
                $randomNumber = mt_rand(1, 10000);
                $world->setRandomNumber($randomNumber);
                $worlds[] = $world;
            }
        }

        $this->entityManager->flush();

        return new JsonResponse($worlds);
    }
}
