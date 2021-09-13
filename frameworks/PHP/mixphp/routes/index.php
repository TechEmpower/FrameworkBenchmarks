<?php

use App\Controller\Benchmark;

return function (Mix\Vega\Engine $vega) {
    $benchmark = new Benchmark();
    $vega->handle('/json', [$benchmark, 'json'])->methods('GET');
    $vega->handle('/plaintext', [$benchmark, 'plaintext'])->methods('GET');
    $vega->handle('/db', [$benchmark, 'db'])->methods('GET');
    $vega->handle('/fortunes', [$benchmark, 'fortunes'])->methods('GET');
    $vega->handle('/update', [$benchmark, 'update'])->methods('GET');
    $vega->handle('/query', [$benchmark, 'query'])->methods('GET');
};
