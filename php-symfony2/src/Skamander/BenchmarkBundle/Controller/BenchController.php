<?php

namespace Skamander\BenchmarkBundle\Controller;

use Symfony\Bundle\FrameworkBundle\Controller\Controller;
use Sensio\Bundle\FrameworkExtraBundle\Configuration\Route;
use Sensio\Bundle\FrameworkExtraBundle\Configuration\Template;
use Symfony\Component\HttpFoundation\JsonResponse;
use Symfony\Component\HttpFoundation\Request;
use Skamander\BenchmarkBundle\Entity\Fortune;

class BenchController extends Controller
{
    /**
     * @Route("/json", name="_json")
     */
    public function jsonAction()
    {
        return new JsonResponse(array('message' => 'Hello World!'));
    }

    /**
     * @Route("/db", name="_db")
     *
     * Used db?queries={queries} instead of db/{queries} to align the test with most of the other tests
     */
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

    /**
     * @Route("/fortunes", name="_fortunes")
     * @Template
     */
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

        return ['fortunes' => $fortunes];
    }
}
