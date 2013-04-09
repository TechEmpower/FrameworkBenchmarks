<?php

/*
 * This file is part of the Silex framework.
 *
 * (c) Fabien Potencier <fabien@symfony.com>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

namespace Silex;

use Symfony\Component\HttpKernel\HttpCache\HttpCache as BaseHttpCache;
use Symfony\Component\HttpFoundation\Request;

/**
 * HTTP Cache extension to allow using the run() shortcut.
 *
 * @author Fabien Potencier <fabien@symfony.com>
 */
class HttpCache extends BaseHttpCache
{
    /**
     * Handles the Request and delivers the Response.
     *
     * @param Request $request The Request object
     */
    public function run(Request $request = null)
    {
        if (null === $request) {
            $request = Request::createFromGlobals();
        }

        $response = $this->handle($request);
        $response->send();
        $this->terminate($request, $response);
    }
}
