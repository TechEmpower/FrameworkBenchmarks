<?php

namespace Doctrine\Tests\Common\Annotations\Fixtures {
    use Doctrine\Tests\Common\Annotations\Fixtures\Annotation\Secure;

    class DifferentNamespacesPerFileWithClassAsFirst {}
}

namespace {
    use Doctrine\Tests\Common\Annotations\Fixtures\Annotation\Route;
}

namespace Doctrine\Tests\Common\Annotations\Fixtures\Foo {
    use Doctrine\Tests\Common\Annotations\Fixtures\Annotation\Template;
}