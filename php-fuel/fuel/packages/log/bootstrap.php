<?php

/**
 * Alias the Log namespace to global so we can overload the Log class
 */
Autoloader::add_core_namespace('Log');

/**
 * Inform the autoloader where to find what...
 */

/**
 * v1.x style classes.
 */
Autoloader::add_classes(array(
	'Log\\Log'                    => __DIR__.'/classes/log.php',
));

/**
 * v2.0 style classes. They are PSR-0, so we only need to define the path.
 */
Autoloader::add_namespace('Psr\\Log', __DIR__.'/Psr/Log/', true);
Autoloader::add_namespace('Monolog', __DIR__.'/src/Monolog/', true);
