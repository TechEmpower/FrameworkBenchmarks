<?php

// namespace Doctrine\Tests\Common\Annotations\Fixtures;
namespace Doctrine\Tests\Common\Annotations\Fixtures\Foo {

    use Doctrine\Tests\Common\Annotations\Fixtures\Annotation\Secure;

    // class NamespaceAndClassCommentedOut {}
}

namespace Doctrine\Tests\Common\Annotations\Fixtures {

    // class NamespaceAndClassCommentedOut {}
    use Doctrine\Tests\Common\Annotations\Fixtures\Annotation\Route;

    // namespace Doctrine\Tests\Common\Annotations\Fixtures;
    use Doctrine\Tests\Common\Annotations\Fixtures\Annotation\Template;

    class NamespaceAndClassCommentedOut {}
}