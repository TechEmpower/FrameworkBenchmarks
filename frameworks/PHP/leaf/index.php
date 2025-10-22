<?php

require __DIR__.'/vendor/autoload.php';

app()->get("/plaintext", fn() => response()->plain('Hello, World!'));

app()->get('/json', fn() => response()->json(['message' => 'Hello, World!']));


app()->run(); // commented with workerman

// Workerman
function run(): string
{
    ob_start();

    app()->run();
    header(HeaderDate::$date); // To pass the bench, nginx auto add it

    return ob_get_clean();
}
