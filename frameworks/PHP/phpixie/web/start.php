<?php
$root = dirname(__DIR__);
$loader = require $root.'/vendor/autoload.php';
$loader->add('', $root.'/classes/');

global $pixie;
$pixie = new \App\Pixie();
$pixie->bootstrap($root);

function run(): string
{
    global $pixie;
    ob_start();

    $pixie->handle_http_request();
    header(HeaderDate::$date); // To pass the bench, nginx auto add it

    return ob_get_clean();
}
