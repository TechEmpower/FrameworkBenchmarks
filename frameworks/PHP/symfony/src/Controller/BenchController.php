<?php

namespace App\Controller;

use Symfony\Bundle\FrameworkBundle\Controller\Controller;
use Symfony\Bundle\FrameworkBundle\Controller\ControllerTrait;
use Symfony\Component\HttpFoundation\JsonResponse;
use Symfony\Component\HttpFoundation\Request;
use Symfony\Component\HttpFoundation\Response;
use App\Entity\Fortune;
use App\Entity\World;

class BenchController extends Controller
{
    use ControllerTrait;

    public function plaintextAction()
    {
        return new Response("Hello, World!", 200, array('Content-Type' => 'text/plain'));
    }

    public function jsonAction()
    {
        return new JsonResponse(array('message' => 'Hello, World!'));
    }

    public function dbAction(Request $request)
    {
        $queries = $request->query->getInt('queries', 1);
        $queries = min(max($queries, 1), 500);

        // possibility for enhancement is the use of SplFixedArray -> http://php.net/manual/de/class.splfixedarray.php
        $worlds = array();
        $repo = $this->getDoctrine()->getRepository(World::class);

        for ($i = 0; $i < $queries; ++$i) {
            $worlds[] = $repo->find(mt_rand(1, 10000));
        }

        if ($queries == 1 && !$request->query->has('queries')) {
            $worlds = $worlds[0];
        }

        return new JsonResponse($worlds);
    }

    public function dbRawAction(Request $request)
    {
        $queries = $request->query->getInt('queries', 1);
        $queries = min(max($queries, 1), 500);

        // possibility for enhancement is the use of SplFixedArray -> http://php.net/manual/de/class.splfixedarray.php
        $worlds = array();
        $conn = $this->get('database_connection');

        for ($i = 0; $i < $queries; ++$i) {
            $worlds[] = $conn->fetchAssoc('SELECT * FROM world WHERE id = ?', array(mt_rand(1, 10000)));
        }

        if ($queries == 1 && !$request->query->has('queries')) {
            $worlds = $worlds[0];
        }

        return new JsonResponse($worlds);
    }

    public function updateAction(Request $request)
    {
      $queries = $request->query->getInt('queries', 1);
      $queries = min(500, max(1, $queries));

      $worlds = array();
      $em = $this->getDoctrine()->getManager();
      $repo = $this->getDoctrine()->getRepository(World::class);

      for ($i = 0; $i < $queries; ++$i) {
        $world = $repo->find(mt_rand(1, 10000));
        $randomNumber = mt_rand(1, 10000);
        $world->setRandomNumber($randomNumber);
        $worlds[] = $world;
      }

      $em->flush();

      return new JsonResponse($worlds);
    }

    public function updateRawAction(Request $request)
    {
      $queries = $request->query->getInt('queries', 1);
      $queries = min(500, max(1, $queries));

      $worlds = array();
      $conn = $this->get('database_connection');

      for ($i = 0; $i < $queries; ++$i) {
          $id = mt_rand(1, 10000);
          $randomNumber = mt_rand(1, 10000);
          $conn->executeUpdate('UPDATE world SET randomNumber=? WHERE id=?', array($randomNumber, $id));
          $worlds[] = array('id' => $id, 'randomNumber' => $randomNumber);
      }

      return new JsonResponse($worlds);
    }

    public function fortunesAction()
    {
        $repo = $this->getDoctrine()->getRepository(Fortune::class);
        $fortunes = $repo->findAll();

        $runtimeFortune = new Fortune();
        $runtimeFortune->setId(0)
            ->setMessage('Additional fortune added at request time.');

        $fortunes[] = $runtimeFortune;

        usort($fortunes, function($left, $right) {
            return strcmp($left->message, $right->message);
        });

        return $this->render("bench/fortunes.html.twig", [
            'fortunes' => $fortunes
        ]);
    }
}
