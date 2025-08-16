<?php

declare(strict_types=1);

namespace App\Http\Controllers;

use Hypervel\Http\Request;

class IndexController extends AbstractController
{
    public function index(Request $request): array
    {
        $user = $request->input('user', 'Hypervel');
        $method = $request->getMethod();

        return [
            'method' => $method,
            'message' => "Hello {$user}.",
        ];
    }
}
