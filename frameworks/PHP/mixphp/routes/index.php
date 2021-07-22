<?php

use App\Controller\Benchmark;

return function (Mix\Vega\Engine $vega) {
    $benchmark = new Benchmark();
    $vega->handleCall('/json', [$benchmark, 'json'])->methods('GET');
    $vega->handleCall('/plaintext', [$benchmark, 'plaintext'])->methods('GET');
    $vega->handleCall('/db', [$benchmark, 'db'])->methods('GET');
    $vega->handleCall('/fortunes', [$benchmark, 'fortunes'])->methods('GET');
    $vega->handleCall('/update', [$benchmark, 'update'])->methods('GET');
    $vega->handleCall('/query', [$benchmark, 'query'])->methods('GET');
};
