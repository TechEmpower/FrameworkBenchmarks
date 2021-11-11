<?php

use Phalcon\Mvc\Controller;

class IndexController extends Controller
{
    public function indexAction(): void
    {
        echo "<h1>Wrong controller for this benchmark!</h1>";
    }
}