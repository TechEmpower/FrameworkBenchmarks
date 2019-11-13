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

    private function getUniqueRandomNumbers($count)
    {
        $res = [];
        do {
            $res[\mt_rand(1, 10000)] = 1;
        } while (\count($res) < $count);

        return \array_keys($res);
    }

    private function flushUpdates(array &$worlds, int $startPosition, int $size)
    {
        $em = $this->entityManager;
        $co = $em->getConnection();
        do {
            try {
                $em->flush();
                $em->clear();
                $done = true;
            } catch (\Exception $e) {
                $done = false;
                if (! $em->isOpen()) {
                    $em = $em->create($co, $em->getConfiguration());
                    $max = \min(\count($worlds), $startPosition + $size);
                    for ($pos = $startPosition; $pos < $max; $pos ++) {
                        $em->merge($worlds[$pos]);
                    }
                }
            }
        } while (! $done);
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
        $numbers = $this->getUniqueRandomNumbers($queries);
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
        $batchSize = 5;
        $queries = (int) $request->query->get('queries', 1);
        $queries = \min(500, \max(1, $queries));

        $worlds = [];
        $lastPosition = 0;

        $numbers = $this->getUniqueRandomNumbers($queries);
        foreach ($numbers as $index=>$id) {
            $world = $this->worldRepository->find($id);
            $world->setRandomNumber(\mt_rand(1, 10000));
            $worlds[] = $world;
            if($index % $batchSize === 0){
                $this->flushUpdates($worlds, $lastPosition, $batchSize);
                $lastPosition = $index + 1;
            }
        }
        if($lastPosition + 1 < $queries){
            $this->flushUpdates($worlds, $lastPosition, $queries - $lastPosition);
        }

        return new JsonResponse($worlds);
    }
}
