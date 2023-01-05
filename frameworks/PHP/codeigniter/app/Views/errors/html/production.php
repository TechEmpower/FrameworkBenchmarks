<!doctype html>
<html>
<head>
    <meta charset="UTF-8">
    <meta name="robots" content="noindex">

    <title>Whoops!</title>

    <style type="text/css">
        <?= preg_replace('#[\r\n\t ]+#', ' ', file_get_contents(__DIR__ . DIRECTORY_SEPARATOR . 'debug.css')) ?>
    </style>
</head>
<body>

    <div class="container text-center">

        <h1 class="headline">Whoops!</h1>

        <p class="lead">We seem to have hit a snag. Please try again later...</p>

    </div>

</body>

</html>
