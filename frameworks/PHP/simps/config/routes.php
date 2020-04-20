<?php

declare(strict_types=1);
/**
 * This file is part of Simps.
 *
 * @link     https://simps.io
 * @document https://doc.simps.io
 * @license  https://github.com/simple-swoole/simps/blob/master/LICENSE
 */

return [
    ['GET', '/json', '\App\Controller\IndexController@index'],
    ['GET', '/plaintext', '\App\Controller\IndexController@plaintext'],
    ['GET', '/fortunes', '\App\Controller\IndexController@fortunes'],
    ['GET', '/db', '\App\Controller\IndexController@db'],
    ['GET', '/updates/[{queries}]', '\App\Controller\IndexController@updates'],
];