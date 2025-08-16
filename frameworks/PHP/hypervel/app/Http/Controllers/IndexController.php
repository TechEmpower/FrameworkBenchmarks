<?php

declare(strict_types=1);

namespace App\Http\Controllers;

use App\Models\Fortune;
use App\Models\World;

class IndexController extends AbstractController
{
    public function json()
    {
        return response()->json(['message' => 'Hello, World!']);
    }

    public function db()
    {
        return response()->json(World::query()->find(self::randomInt()));
    }

    public function queries($queries = 1)
    {
        $queries = self::clamp($queries);

        $rows = [];
        while ($queries--) {
            $rows[] = World::query()->find(self::randomInt());
        }

        return response()->json($rows);
    }

    public function fortunes()
    {
        $rows = Fortune::all();

        $insert = new Fortune();
        $insert->id = 0;
        $insert->message = 'Additional fortune added at request time.';

        $rows->add($insert);
        $rows = $rows->sortBy('message');

        return response(view('fortunes', ['rows' => $rows]), 200, ['Content-Type' => 'text/html; charset=UTF-8']);
    }

    public function updates($queries = 1)
    {
        $queries = self::clamp($queries);

        $rows = [];

        while ($queries--) {
            $row = World::query()->find(self::randomInt());
            while (($randomInt = self::randomInt()) === $row->randomNumber) {
            }
            $row->randomNumber = $randomInt;
            $row->save();

            $rows[] = $row;
        }

        return response()->json($rows);
    }

    public function plaintext()
    {
        return response('Hello, World!', 200, ['Content-Type' => 'text/plain']);
    }

    private static function randomInt()
    {
        return random_int(1, 10000);
    }

    private static function clamp($value)
    {
        if (!is_numeric($value) || $value < 1) {
            return 1;
        }
        if ($value > 500) {
            return 500;
        }
        return (int)$value;
    }
}
