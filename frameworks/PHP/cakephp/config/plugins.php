<?php
/**
 * Plugin configuration.
 *
 * In this file, you configure which plugins are loaded in the different states your app can be.
 * It's loaded via the `parent::bootstrap();` call inside your `Application::bootstrap()` method.
 * For more information see https://book.cakephp.org/5/en/plugins.html#loading-plugins-via-configuration-array
 *
 * CakePHP(tm) : Rapid Development Framework (https://cakephp.org)
 * Copyright (c) Cake Software Foundation, Inc. (https://cakefoundation.org)
 *
 * Licensed under The MIT License
 * For full copyright and license information, please see the LICENSE.txt
 * Redistributions of files must retain the above copyright notice.
 *
 * @copyright     Copyright (c) Cake Software Foundation, Inc. (https://cakefoundation.org)
 * @link          https://cakephp.org CakePHP(tm) Project
 * @since         5.0.0
 * @license       https://opensource.org/licenses/mit-license.php MIT License
 */

return [
    // Plugins only needed when in debug mode
    //'DebugKit' => ['onlyDebug' => true],

    // Optional plugins which are only needed in CLI commands
    //'Bake' => ['onlyCli' => true, 'optional' => true],

    // Required plugins only in CLI commands
    //'Migrations' => ['onlyCli' => true],

    // Add your custom plugins here
];
