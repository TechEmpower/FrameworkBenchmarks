<?php

declare(strict_types=1);
/**
 * This file is part of Simps.
 *
 * @link     https://simps.io
 * @document https://doc.simps.io
 * @license  https://github.com/simple-swoole/simps/blob/master/LICENSE
 */

namespace App\Controller;

use App\Model\DbModel;
use App\Model\MicroModel;
use Simps\Server\Protocol\HTTP\SimpleResponse;

class IndexController
{
    public function index($server, $fd)
    {
        $server->send(
            $fd,
            SimpleResponse::build(
                json_encode(['message' => 'Hello, World!']),
                200,
                ['Content-Type' => 'application/json', 'Date' => gmdate("D, d M Y H:i:s T")]
            )
        );
    }

    public function plaintext($server, $fd)
    {
        $server->send(
            $fd,
            SimpleResponse::build(
                'Hello, World!',
                200,
                ['Content-Type' => 'text/plain', 'Date' => gmdate("D, d M Y H:i:s T")]
            )
        );
    }

    public function fortunes($server, $fd)
    {
        $db = new DbModel();
        $fortune = $db->fortunes();
        $html = '';
        foreach ($fortune as $id => $message) {
            $message = \htmlspecialchars($message, ENT_QUOTES, 'UTF-8');
            $html .= "<tr><td>{$id}</td><td>{$message}</td></tr>";
        }

        $data = '<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>'
            . $html .
            '</table></body></html>';

        $server->send(
            $fd,
            SimpleResponse::build(
                $data,
                200,
                ['Content-Type' => 'text/html; charset=utf-8', 'Date' => gmdate("D, d M Y H:i:s T")]
            )
        );
    }

    public function db($server, $fd, $data)
    {
        $db = new DbModel();
        if (isset($data['queries'])) {
            $res = $db->db((int)$data['queries']);
        } else {
            $res = $db->db(-1);
        }

        $server->send(
            $fd,
            SimpleResponse::build(
                $res,
                200,
                ['Content-Type' => 'application/json', 'Date' => gmdate("D, d M Y H:i:s T")]
            )
        );
    }

    public function queries($server, $fd, $data)
    {
        $db = new DbModel();
        if (isset($data['queries'])) {
            $res = $db->db((int)$data['queries']);
        } else {
            $res = $db->db();
        }

        $server->send(
            $fd,
            SimpleResponse::build(
                $res,
                200,
                ['Content-Type' => 'application/json', 'Date' => gmdate("D, d M Y H:i:s T")]
            )
        );
    }

    public function updates($server, $fd, $data)
    {
        $db = new DbModel();
        if (isset($data['queries'])) {
            $res = $db->updates((int)$data['queries']);
        } else {
            $res = $db->updates(-1);
        }

        $server->send(
            $fd,
            SimpleResponse::build(
                $res,
                200,
                ['Content-Type' => 'application/json', 'Date' => gmdate("D, d M Y H:i:s T")]
            )
        );
    }

    public function microDb($server, $fd)
    {
        $db = new MicroModel();
        $res = $db->microDb();

        $server->send(
            $fd,
            SimpleResponse::build(
                $res,
                200,
                ['Content-Type' => 'application/json', 'Date' => gmdate("D, d M Y H:i:s T")]
            )
        );
    }

    public function microQueries($server, $fd, $data)
    {
        $db = new MicroModel();
        if (isset($data['queries'])) {
            $res = $db->microQueries((int)$data['queries']);
        } else {
            $res = $db->microQueries();
        }

        $server->send(
            $fd,
            SimpleResponse::build(
                $res,
                200,
                ['Content-Type' => 'application/json', 'Date' => gmdate("D, d M Y H:i:s T")]
            )
        );
    }

    public function microUpdates($server, $fd, $data)
    {
        $db = new MicroModel();
        if (isset($data['queries'])) {
            $res = $db->microUpdates((int)$data['queries']);
        } else {
            $res = $db->microUpdates();
        }

        $server->send(
            $fd,
            SimpleResponse::build(
                $res,
                200,
                ['Content-Type' => 'application/json', 'Date' => gmdate("D, d M Y H:i:s T")]
            )
        );
    }
}