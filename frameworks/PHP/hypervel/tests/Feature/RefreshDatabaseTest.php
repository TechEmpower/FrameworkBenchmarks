<?php

declare(strict_types=1);

namespace Tests\Feature;

use App\Models\User;
use Hypervel\Foundation\Testing\RefreshDatabase;
use Tests\TestCase;

/**
 * @internal
 * @coversNothing
 */
class RefreshDatabaseTest extends TestCase
{
    use RefreshDatabase;

    public function testCreateUser()
    {
        $user = factory(User::class)->create();

        $this->assertDatabaseHas('users', [
            'id' => $user->id,
        ]);
    }

    public function testCreateMultipleUsers()
    {
        factory(User::class, $count = 5)->create();

        $this->assertDatabaseCount('users', $count);
    }

    public function testZeroUserAfterRefresh()
    {
        $this->assertSame(0, User::count());
    }
}
