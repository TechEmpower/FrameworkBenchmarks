<?php

use mako\application\web\Application;

/**
 * Include the application init file.
 */
include dirname(__DIR__) . '/app/init.php';

/*
 * Start and run the application.
 */
Application::start(MAKO_APPLICATION_PATH)->run();
