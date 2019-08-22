<?php

namespace App\Controller;

use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\Routing\Annotation\Route;

class PlaintextController
{
    /**
     * @Route("/plaintext")
     */
    public function plaintext(): Response
    {
        return new Response('Hello, World!', 200, ['Content-Type' => 'text/plain']);
    }
}
