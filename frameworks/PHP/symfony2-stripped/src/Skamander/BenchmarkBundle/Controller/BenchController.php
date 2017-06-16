<?php

namespace Skamander\BenchmarkBundle\Controller;

use Symfony\Bundle\FrameworkBundle\Controller\Controller;
use Symfony\Component\HttpFoundation\JsonResponse;
use Symfony\Component\HttpFoundation\Request;
use Symfony\Component\HttpFoundation\Response;
use Skamander\BenchmarkBundle\Entity\Fortune;

class BenchController extends Controller
{

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
        $queries = is_numeric($queries) ? min(max($queries, 1), 500) : 1;

        // possibility for enhancement is the use of SplFixedArray -> http://php.net/manual/de/class.splfixedarray.php
        $worlds = array();
        $repo = $this->getDoctrine()
            ->getRepository('SkamanderBenchmarkBundle:World');

        for ($i = 0; $i < $queries; ++$i) {
            $worlds[] =  $repo->find(mt_rand(1, 10000));
        }

        if ($queries == 1 && !$request->query->has('queries')) {
            $worlds = $worlds[0];
        }

        return new JsonResponse($worlds);
    }

    public function dbRawAction(Request $request)
    {
        $queries = $request->query->getInt('queries', 1);
        $queries = is_numeric($queries) ? min(max($queries, 1), 500) : 1;

        // possibility for enhancement is the use of SplFixedArray -> http://php.net/manual/de/class.splfixedarray.php
        $worlds = array();
        $conn = $this->get('database_connection');

        for($i = 0; $i < $queries; ++$i) {
            $worlds[] =  $conn->fetchAssoc('SELECT * FROM World WHERE id = ?', array(mt_rand(1, 10000)));
        }

        if ($queries == 1) {
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
      $repo = $this->getDoctrine()
          ->getRepository('SkamanderBenchmarkBundle:World');

      for ($i = 0; $i < $queries; ++$i) {
        $world = $repo->find(mt_rand(1, 10000));
        $random_number = mt_rand(1, 10000);
        $world->setRandomNumber($random_number);
        $em->persist($world);
        $worlds[] =  $world;
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

      for($i = 0; $i < $queries; ++$i) {
          $id = mt_rand(1, 10000);
          $random_number = mt_rand(1, 10000);
          $conn->executeUpdate('UPDATE World SET randomNumber=? WHERE id=?', array($random_number, $id));
          $worlds[] =  array('id' => $id, 'randomNumber' => $random_number);
      }

      return new JsonResponse($worlds);
    }

    public function fortunesAction()
    {
        $repo = $this->getDoctrine()
            ->getRepository('SkamanderBenchmarkBundle:Fortune');
        $fortunes = $repo->findAll();

        $runtimeFortune = new Fortune();
        $runtimeFortune->setId(0)
            ->setMessage('Additional fortune added at request time.');

        $fortunes[] = $runtimeFortune;

        usort($fortunes, function($left, $right) {
            return strcmp($left->message, $right->message);
        });

        return $this->render("SkamanderBenchmarkBundle:Bench:fortunes.html.twig", [
            'fortunes' => $fortunes
        ]);
    }

    public function fortunesPhpAction()
    {
        $repo = $this->getDoctrine()
            ->getRepository('SkamanderBenchmarkBundle:Fortune');
        $fortunes = $repo->findAll();

        $runtimeFortune = new Fortune();
        $runtimeFortune->setId(0)
            ->setMessage('Additional fortune added at request time.');

        $fortunes[] = $runtimeFortune;

        usort($fortunes, function($left, $right) {
            return strcmp($left->message, $right->message);
        });

        return $this->render("SkamanderBenchmarkBundle:Bench:fortunes.html.php", [
            'fortunes' => $fortunes
        ]);
    }

    public function fortunesRawAction()
    {
        $repo = $this->getDoctrine()
            ->getRepository('SkamanderBenchmarkBundle:Fortune');
        $fortunes = $repo->findAll();

        $runtimeFortune = new Fortune();
        $runtimeFortune->setId(0)
            ->setMessage('Additional fortune added at request time.');

        $fortunes[] = $runtimeFortune;

        usort($fortunes, function($left, $right) {
            return strcmp($left->message, $right->message);
        });

        // This is not the symfony way to work with templates! It's implemented to show users
        // who don't want to use template engines (like twig), or template sugar (like the slots etc.
        // from symfony 2), because in their opinion already built-in php constructs like foreach +
        // if else + include etc. are enough, that the performance impact should be neglectable, and
        // that the advantages outweigh the disadvantages (performance).
        $title = 'Fortunes';

        ob_start();
        include __DIR__ . '/../Resources/views/Bench/raw/content.php';
        $response = ob_get_clean();

        return new Response($response);
    }
}
