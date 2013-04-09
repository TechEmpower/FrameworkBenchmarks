<?php

/*
 * This file is part of the Silex framework.
 *
 * (c) Fabien Potencier <fabien@symfony.com>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

namespace Silex\Application;

/**
 * Swiftmailer trait.
 *
 * @author Fabien Potencier <fabien@symfony.com>
 */
trait SwiftmailerTrait
{
    /**
     * Sends an email.
     *
     * @param \Swift_Message $message A \Swift_Message instance
     */
    public function mail(\Swift_Message $message)
    {
        return $this['mailer']->send($message);
    }
}
