<?php

namespace Doctrine\Tests\Common\Annotations\Fixtures;

/**
 * @since 2.0
 * @version $Id: SomeEntityClass.php 509 2012-02-03 09:38:48Z mf $
 */
class ClassDDC1660
{

    /**
     * @var     string
     * @since   2.0
     * @version 1
     */
    public $foo;

    /**
     * @param   string
     * @return  string
     * @since   2.0
     * @version 1
     */
    public function bar($param)
    {
        return null;
    }

}