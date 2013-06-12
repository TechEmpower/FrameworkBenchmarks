<?php
$root = dirname(__DIR__);
$loader = require $root.'/vendor/autoload.php';
$loader->add('', $root.'/classes/');
$loader->add('PHPixie', $root.'/vendor/phpixie/core/classes/');
$loader->add('PHPixie', $root.'/vendor/phpixie/db/classes/');
$loader->add('PHPixie',$root.'/vendor/phpixie/orm/classes/');

$pixie = new \App\Pixie();
$pixie->bootstrap($root)->http_request()->execute()->send_headers()->send_body();
?>