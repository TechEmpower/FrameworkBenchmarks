<?php
/**
 * This file is part of webman.
 *
 * Licensed under The MIT License
 * For full copyright and license information, please see the MIT-LICENSE.txt
 * Redistributions of files must retain the above copyright notice.
 *
 * @author    walkor<walkor@workerman.net>
 * @copyright walkor<walkor@workerman.net>
 * @link      http://www.workerman.net/
 * @license   http://www.opensource.org/licenses/mit-license.php MIT License
 */

use Webman\Route;


Route::any('/json', 'app\controller\Index@json');
Route::any('/plaintext', 'app\controller\Index@plaintext');
Route::any('/fortunes', 'app\controller\Index@fortunes');
Route::any('/db', 'app\controller\Index@db');
Route::any('/queries[/[{q}]]', 'app\controller\Index@queries');
Route::any('/updates[/[{q}]]', 'app\controller\Index@updates');
