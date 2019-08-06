<?php

namespace App\Controller;

use App\Entity\Fortune;
use App\Entity\World;
use Exception;
use Symfony\Bundle\FrameworkBundle\Controller\AbstractController;
use Symfony\Component\HttpFoundation\JsonResponse;
use Symfony\Component\HttpFoundation\Request;
use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\Routing\Annotation\Route;

class BenchController extends AbstractController
{
    /**
     * @Route("/plaintext")
     */
    public function plaintext(): Response
    {
        return new Response('Hello, World!', 200, ['Content-Type' => 'text/plain']);
    }

    /**
     * @Route("/json")
     */
    public function json(): JsonResponse
    {
        return new JsonResponse(['message' => 'Hello, World!']);
    }

    /**
     * @Route("/db")
     * @throws Exception
     */
    public function db(Request $request): JsonResponse
    {
        $queries = $request->query->getInt('queries', 1);
        $queries = min(max($queries, 1), 500);

        // possibility for enhancement is the use of SplFixedArray -> http://php.net/manual/de/class.splfixedarray.php
        $worlds = [];
        $repo = $this->getDoctrine()->getRepository(World::class);

        for ($i = 0; $i < $queries; ++$i) {
            $worlds[] = $repo->find(random_int(1, 10000));
        }

        if ($queries === 1 && !$request->query->has('queries')) {
            $worlds = $worlds[0];
        }

        return new JsonResponse($worlds);
    }

    /**
     * @Route("/db-raw")
     * @throws Exception
     */
    public function dbRaw(Request $request): JsonResponse
    {
        $queries = $request->query->getInt('queries', 1);
        $queries = min(max($queries, 1), 500);

        // possibility for enhancement is the use of SplFixedArray -> http://php.net/manual/de/class.splfixedarray.php
        $worlds = [];
        $conn = $this->get('database_connection');

        for ($i = 0; $i < $queries; ++$i) {
            $worlds[] = $conn->fetchAssoc('SELECT * FROM world WHERE id = ?', [random_int(1, 10000)]);
        }

        if ($queries === 1 && !$request->query->has('queries')) {
            $worlds = $worlds[0];
        }

        return new JsonResponse($worlds);
    }

    /**
     * @Route("/update")
     * @throws Exception
     */
    public function update(Request $request): JsonResponse
    {
      $queries = $request->query->getInt('queries', 1);
      $queries = min(500, max(1, $queries));

      $worlds = [];
      $em = $this->getDoctrine()->getManager();
      $repo = $this->getDoctrine()->getRepository(World::class);

      for ($i = 0; $i < $queries; ++$i) {
        $world = $repo->find(random_int(1, 10000));
        if ($world) {
            $randomNumber = random_int(1, 10000);
            $world->setRandomNumber($randomNumber);
            $worlds[] = $world;
        }
      }

      $em->flush();

      return new JsonResponse($worlds);
    }

    /**
     * @Route("/update-raw")
     * @throws Exception
     */
    public function updateRaw(Request $request): JsonResponse
    {
      $queries = $request->query->getInt('queries', 1);
      $queries = min(500, max(1, $queries));

      $worlds = [];
      $conn = $this->get('database_connection');

      for ($i = 0; $i < $queries; ++$i) {
          $id = random_int(1, 10000);
          $randomNumber = random_int(1, 10000);
          $conn->executeUpdate('UPDATE world SET randomNumber=? WHERE id=?', [$randomNumber, $id]);
          $worlds[] = ['id' => $id, 'randomNumber' => $randomNumber];
      }

      return new JsonResponse($worlds);
    }

    /**
     * @Route("/fortunes")
     */
    public function fortunes(): Response
    {
        $repo = $this->getDoctrine()->getRepository(Fortune::class);
        $fortunes = $repo->findAll();

        $runtimeFortune = new Fortune();
        $runtimeFortune->setId(0);
        $runtimeFortune->setMessage('Additional fortune added at request time.');

        $fortunes[] = $runtimeFortune;

        usort($fortunes, static function($left, $right) {
            return strcmp($left->message, $right->message);
        });

        return $this->render(
            'bench/fortunes.html.twig', [
            'fortunes' => $fortunes
        ]);
    }
}
