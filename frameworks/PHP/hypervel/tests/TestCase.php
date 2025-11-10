<?php

declare(strict_types=1);

namespace Tests;

use Hypervel\Foundation\Testing\Concerns\RunTestsInCoroutine;
use Hypervel\Foundation\Testing\TestCase as BaseTestCase;

abstract class TestCase extends BaseTestCase
{
    use RunTestsInCoroutine;
}
