<?php

namespace Skamander\BenchmarkBundle\Controller;

use Symfony\Bundle\FrameworkBundle\Controller\Controller;
use Sensio\Bundle\FrameworkExtraBundle\Configuration\Route;
use Symfony\Component\HttpFoundation\JsonResponse;
use Symfony\Component\HttpFoundation\Request;

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
}
