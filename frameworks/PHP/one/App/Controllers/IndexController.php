<?php

namespace App\Controllers;

use App\Model\Fortune;
use App\Model\World;
use One\Http\Controller;

class IndexController extends Controller
{

    protected function json($data)
    {
        $this->response->header('Content-type', 'application/json; charset=UTF-8');
        return json_encode($data);
    }

    public function toJson()
    {
        return $this->json(['message' => 'Hello, World!']);
    }

    public function plaintext()
    {
        $this->response->header('Content-Type', 'text/plain');
        return 'Hello, World!';
    }

    public function db()
    {
        return $this->json(World::find(mt_rand(1, 10000)));
    }

    public function fortunes()
    {
        $arr   = Fortune::findAll();
        $rs    = [];
        $rs[0] = 'Additional fortune added at request time.';
        foreach ($arr as $item) {
            $rs[$item['id']] = $item['message'];
        }
        asort($rs);

        $html = '';
        foreach ($rs as $id => $message) {
            $message = htmlspecialchars($message, ENT_QUOTES, 'UTF-8');
            $html    .= "<tr><td>{$id}</td><td>{$message}</td></tr>";
        }

        $this->response->header('Content-type', 'text/html; charset=UTF-8');
        return "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>{$html}</table></body></html>";
    }

    public function updates($count = 1)
    {
        $count = max(min(intval($count), 500), 1);
        $list  = [];
        while ($count--) {
            $row    = World::find(mt_rand(1, 10000));
            $list[] = $row;
            $row->update(['randomNumber' => mt_rand(1, 10000)]);
        }
        return $this->json($list);
    }

    public function queries($count = 1)
    {
        $count = max(min(intval($count), 500), 1);
        $list  = [];
        while ($count--) {
            $list[] = World::find(mt_rand(1, 10000));
        }
        return $this->json($list);

    }
}




