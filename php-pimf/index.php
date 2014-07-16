<?php
/*
|--------------------------------------------------------------------------
| PIMF Application gateway/runner
|--------------------------------------------------------------------------
*/
include_once 'app/bootstrap.app.php';

use \Pimf\Application as App;

App::run($_GET, $_POST, $_COOKIE);

App::finish();