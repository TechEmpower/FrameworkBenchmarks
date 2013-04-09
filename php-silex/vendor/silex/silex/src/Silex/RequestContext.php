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

use Symfony\Component\Routing\RequestContext as BaseRequestContext;
use Symfony\Component\HttpFoundation\Request;

/**
 * Request Context for Symfony <= 2.2
 *
 * @author Fabien Potencier <fabien@symfony.com>
 */
class RequestContext extends BaseRequestContext
{
    public function fromRequest(Request $request)
    {
        parent::fromRequest($request);

        // Inject the query string as a parameter for Symfony versions <= 2.2
        if (!method_exists($this, 'getQueryString') && '' !== $qs = $request->server->get('QUERY_STRING')) {
            $this->setParameter('QUERY_STRING', $qs);
        }
    }
}
