<?php
/**
 * Spiral Framework.
 *
 * @license   MIT
 * @author    Anton Titov (Wolfy-J)
 */
declare(strict_types=1);

namespace App;

use App\Bootloader\DebugBootloader;
use App\Bootloader\RoutesBootloader;
use Spiral\Bootloader;
use Spiral\DotEnv\Bootloader as DotEnv;
use Spiral\Framework\Kernel;
use Spiral\Nyholm\Bootloader as Nyholm;
use Spiral\Stempler\Bootloader as Stempler;

class App extends Kernel
{
    /*
     * List of components and extensions to be automatically registered
     * within system container on application start.
     */
    protected const LOAD = [
        // Environment configuration
        DotEnv\DotenvBootloader::class,

        // Core Services
        Bootloader\DebugBootloader::class,
        Bootloader\SnapshotsBootloader::class,

        // Security and validation
        Bootloader\Security\EncrypterBootloader::class,
        Bootloader\Security\ValidationBootloader::class,
        Bootloader\Security\FiltersBootloader::class,
        Bootloader\Security\GuardBootloader::class,

        Bootloader\Http\HttpBootloader::class,
        DebugBootloader::class,

        // HTTP extensions
        Nyholm\NyholmBootloader::class,
        Bootloader\Http\RouterBootloader::class,
        Bootloader\Http\ErrorHandlerBootloader::class,

        // Databases
        Bootloader\Database\DatabaseBootloader::class,
        Bootloader\Database\MigrationsBootloader::class,

        // ORM
        Bootloader\Cycle\CycleBootloader::class,
        Bootloader\Cycle\AnnotatedBootloader::class,

        // Template engine
        Stempler\StemplerBootloader::class,

        // Framework commands
        Bootloader\CommandBootloader::class
    ];

    /*
     * Application specific services and extensions.
     */
    protected const APP = [
        RoutesBootloader::class,
    ];
}