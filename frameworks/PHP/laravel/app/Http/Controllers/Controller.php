<?php

namespace App\Http\Controllers;

use App\Models\Fortune;
use App\Models\World;
use Illuminate\Routing\Controller as BaseController;

class Controller extends BaseController
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

        return view('fortunes', ['rows' => $rows]);
    }

    public function updates($queries = 1)
    {
        $queries = self::clamp($queries);

        $rows = [];

        while ($queries--) {
            $row = World::query()->find(self::randomInt());
            $row->randomNumber = self::randomInt();
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
