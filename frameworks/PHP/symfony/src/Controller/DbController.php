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
     */
    public function db(): JsonResponse
    {
        return new JsonResponse($this->worldRepository->find(mt_rand(1, 10000)));
    }

    /**
     * @Route("/queries")
     */
    public function queries(Request $request): JsonResponse
    {
        $queries = (int) $request->query->get('queries', 1);
        $queries = min(max($queries, 1), 500);

        // possibility for enhancement is the use of SplFixedArray -> http://php.net/manual/de/class.splfixedarray.php
        $worlds = [];
        // Numbers must be unique, otherwise there is a chance to fetch twice the same number, Doctrine will then re-use
        // the same object and won't perform a second request which is forbidden
        foreach ($this->getUniqueRandomNumbers($queries) as $id) {
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
        $queries = min(500, max(1, $queries));

        $worlds = [];
        // Numbers must be unique, otherwise there is a chance to fetch twice the same number, Doctrine will then re-use
        // the same object and won't perform a second request which is forbidden
        $ids = $this->getUniqueRandomNumbers($queries);
        // Numbers must be ordered to avoid deadlock when 2 process will update the same ids in a random order
        sort($ids);
        foreach ($ids as $id) {
            $worlds[] = $world = $this->worldRepository->find($id);

            // The new value have to be different from the previous. Otherwise Doctrine won't execute the update query
            // which is forbidden
            $oldId = $world->randomNumber;
            do {
                $newId = mt_rand(1, 10000);
            } while($oldId === $newId);
            $world->randomNumber = $newId;
        }
        $this->entityManager->flush();

        return new JsonResponse($worlds);
    }

    private function getUniqueRandomNumbers($count)
    {
        $res = [];
        $current = 0;
        do {
            for ($i=$current; $i<$count; $i++) {
                $res[mt_rand(1, 10000)] = 1;
            }
        } while (($current = count($res)) < $count);

        return array_keys($res);
    }
}
