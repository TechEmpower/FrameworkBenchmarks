<?php
/**
 * The Front Controller for handling every request
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
 * @since         0.2.9
 * @license       MIT License (https://opensource.org/licenses/mit-license.php)
 */


//require __DIR__ . '/vendor/autoload.php';

use App\Application;
use Cake\Http\Server;

global $server;

// Bind your application to the server.
$server = new Server(new Application(__DIR__ . '/config'));


function run(): string
{
    global $server;
    ob_start();

    // Run the request/response through the application and emit the response.
    $server->emit($server->run());
    header(HeaderDate::$date); // To pass the bench, nginx auto add it

    return ob_get_clean();
}
