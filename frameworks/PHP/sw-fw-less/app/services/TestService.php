<?php

namespace App\services;

use App\models\Fortune;
use App\models\World;
use SwFwLess\components\http\Response;
use SwFwLess\services\BaseService;

class TestService extends BaseService
{
    public function json()
    {
        return ['message' => 'Hello, World!'];
    }

    public function db()
    {
        return World::select()->cols(['*'])->where('`id` = :id')
            ->bindValue(':id', random_int(1, 10000))
            ->first()
            ->toArray();
    }

    public function queries($queries = 1)
    {
        $queries = $this->clamp($queries);

        $rows = [];
        while ($queries--) {
            $rows[] = World::select()->cols(['*'])->where('`id` = :id')
                ->bindValue(':id', random_int(1, 10000))
                ->first()
                ->toArray();
        }

        return $rows;
    }

    public function fortunes()
    {
        $rows = Fortune::select()->cols(['*'])->get();

        $insert = new Fortune();
        $insert->id = 0;
        $insert->message = 'Additional fortune added at request time.';

        $rows[] = $insert;

        usort($fortunes, function ($left, $right) {
            return strcmp($left->message, $right->message);
        });

        return Response::output($this->renderFortunes($fortunes), 200, ['Content-Type' => 'text/html']);
    }

    private function renderFortunes($fortunes)
    {
        $html = <<<EOF
<!DOCTYPE html>
<html>
<head><title>Fortunes</title></head>
<body>
<table>
	<tr><th>id</th><th>message</th></tr>

	%s
</table>
</body>
</html>
EOF;

        $fortuneRows = '';
        foreach ($fortunes as $fortune) {
            $fortuneRows .= '	<tr><td>' . $fortune->id . '</td><td>' . $fortune->message . '</td></tr>' . PHP_EOL;
        }

        return sprintf($html, $fortuneRows);
    }

    public function updates($queries = 1)
    {
        $queries = $this->clamp($queries);

        $rows = [];

        while ($queries--) {
            $row = World::select()->cols(['*'])->where('`id` = :id')
                ->bindValue(':id', random_int(1, 10000))
                ->first();
            $row->randomNumber = random_int(1, 10000);
            $row->save();

            $rows[] = $row->toArray();
        }

        return $rows;
    }

    public function plaintext()
    {
        return Response::output('Hello, World!', 200, ['Content-Type' => 'text/plain']);
    }

    private function clamp($value): int
    {
        if (! is_numeric($value) || $value < 1) {
            return 1;
        }
        if ($value > 500) {
            return 500;
        }
        return (int)$value;
    }
}
