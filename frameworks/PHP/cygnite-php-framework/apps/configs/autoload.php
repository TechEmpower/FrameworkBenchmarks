<?php
/**
 * This file is part of the Cygnite package.
 *
 * (c) Sanjoy Dey <dey.sanjoy0@gmail.com>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */
namespace Cygnite\Foundation;

if (!defined('CF_SYSTEM')) {
    exit('External script access not allowed');
}

return array(
    /*---------------------------------------------------------------------------
    * Register all your directories to auto load your files.
    *---------------------------------------------------------------------------
    * You can specify multiple numbers of directories here to register on
    * Cygnite Engine during runtime. Don't worry about the application
    * performance because all libraries are lazy loaded. But filename,
    * class name and file should be same, StudlyCaps.
    *
    *  Specify your directory path here. That's all. Cygnite will
    *  take care of rest.
    */
    Application::instance()->registerDirectories(
        array(
            'apps.controllers',
            'apps.models',
            'apps.configs.definitions'
        )
    )
);