<?php

namespace Doctrine\Tests\Common\Annotations\Fixtures;

$var = 1;
function () use ($var) {};

use Doctrine\Tests\Common\Annotations\Fixtures\Annotation\Secure;
use Doctrine\Tests\Common\Annotations\Fixtures\Annotation\Route;
use Doctrine\Tests\Common\Annotations\Fixtures\Annotation\Template;

$var = 1;
function () use ($var) {};

class NamespaceWithClosureDeclaration {}