<?php
namespace app\modules\site\controllers;

use HttpSoft\Message\StreamFactory;
use Psr\Http\Message\ResponseInterface;

class DefaultController extends \Piko\Controller
{
    /**
     * Return a Hello World! in text/plain
     *
     * @return ResponseInterface
     */
    public function plaintextAction(): ResponseInterface
    {
        $body = (new StreamFactory())->createStream('Hello, World!');

        return $this->response->withHeader('Content-Type', 'text/plain; charset=UTF-8')->withBody($body);
    }

    /**
     * Return a json response with a Hello, World! message
     *
     * @return ResponseInterface
     */
    public function jsonAction(): ResponseInterface
    {
        return $this->jsonResponse(['message' => 'Hello, World!']);
    }
}
