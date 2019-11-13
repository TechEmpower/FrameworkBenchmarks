<?php

namespace App\Controller;

use Symfony\Component\HttpFoundation\JsonResponse;
use Symfony\Component\Routing\Annotation\Route;

class JsonController
{
    /**
     * @Route("/json")
     */
    public function json(): JsonResponse
    {
        return new JsonResponse(['message' => 'Hello, World!']);
    }
}
