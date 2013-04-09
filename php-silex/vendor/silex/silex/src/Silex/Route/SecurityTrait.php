<?php

/*
 * This file is part of the Silex framework.
 *
 * (c) Fabien Potencier <fabien@symfony.com>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

namespace Silex\Route;

use Symfony\Component\Security\Core\Exception\AccessDeniedException;

/**
 * Security trait.
 *
 * @author Fabien Potencier <fabien@symfony.com>
 */
trait SecurityTrait
{
    public function secure($roles)
    {
        $this->before(function ($request, $app) use ($roles) {
            if (!$app['security']->isGranted($roles)) {
                throw new AccessDeniedException();
            }
        });
    }
}
