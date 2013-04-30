<?php

namespace Skamander\BenchmarkBundle\Controller;

use Symfony\Bundle\FrameworkBundle\Controller\Controller;
use Symfony\Component\HttpFoundation\JsonResponse;
use Symfony\Component\HttpFoundation\Request;
use Skamander\BenchmarkBundle\Entity\Fortune;

class BenchController extends Controller
{

    public function jsonAction()
    {
        return new JsonResponse(array('message' => 'Hello World!'));
    }

    public function dbAction(Request $request)
    {
        $queries = $request->query->getInt('queries', 1);

        // possibility for enhancement is the use of SplFixedArray -> http://php.net/manual/de/class.splfixedarray.php
        $worlds = array();
        $repo = $this->getDoctrine()
            ->getRepository('SkamanderBenchmarkBundle:World');

        for($i = 0; $i < $queries; ++$i) {
            $worlds[] =  $repo->find(mt_rand(1, 10000));
        }

        return new JsonResponse($worlds);
    }

    public function dbRawAction(Request $request)
    {
        $queries = $request->query->getInt('queries', 1);

        // possibility for enhancement is the use of SplFixedArray -> http://php.net/manual/de/class.splfixedarray.php
        $worlds = array();
        $conn = $this->get('database_connection');

        for($i = 0; $i < $queries; ++$i) {
            $worlds[] =  $conn->fetchAssoc('SELECT * FROM World WHERE id = ?', array(mt_rand(1, 10000)));
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
            if ($left->message === $right->message) {
                return 0;
            } else if ($left->message > $right->message) {
                return 1;
            } else {
                return -1;
            }
        });

        return $this->render("SkamanderBenchmarkBundle:Bench:fortunes.html.twig", [
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
            if ($left->message === $right->message) {
                return 0;
            } else if ($left->message > $right->message) {
                return 1;
            } else {
                return -1;
            }
        });

        return $this->render("SkamanderBenchmarkBundle:Bench:fortunes.html.php", [
            'fortunes' => $fortunes
        ]);
    }
}
