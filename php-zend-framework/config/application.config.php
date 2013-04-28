<?php
return array(
    'modules' => array(
        'Application',
    ),
    'module_listener_options' => array(
        'module_paths' => array(
            './module',
        ),
        'config_glob_paths' => array(
            'config/autoload/{,*.}{global,local}.php',
        ),
        'config_cache_enabled' => true,
        'config_cache_key'     => 'config_cache',
        'cache_dir'            => 'data/cache',
        'check_dependencies'   => false,
    ),
);