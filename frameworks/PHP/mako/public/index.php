<?php

use mako\application\CurrentApplication;
use mako\application\web\Application;

/**
 * Include the application init file.
 */
include dirname(__DIR__) . '/app/init.php';

/*
 * Start and run the application.
 */
CurrentApplication::set(new Application(MAKO_APPLICATION_PATH))->run();
