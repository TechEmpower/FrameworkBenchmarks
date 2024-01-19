<?php

declare(strict_types=1);

namespace App;

use App\Bootloader\RoutesBootloader;
use Spiral\Boot\Bootloader\CoreBootloader;
use Spiral\Bootloader;
use Spiral\DotEnv\Bootloader as DotEnv;
use Spiral\Framework\Kernel;
use Spiral\Nyholm\Bootloader as Nyholm;
use Spiral\Cycle\Bootloader as CycleBridge;
use Spiral\RoadRunnerBridge\Bootloader as RoadRunnerBridge;
use Spiral\Stempler\Bootloader as Stempler;
use Spiral\Scaffolder\Bootloader as Scaffolder;
use Spiral\Tokenizer\Bootloader\TokenizerListenerBootloader;

class App extends Kernel
{
    protected const SYSTEM = [
        CoreBootloader::class,
        TokenizerListenerBootloader::class,
        // Environment configuration
        DotEnv\DotenvBootloader::class,
    ];

    /*
     * List of components and extensions to be automatically registered
     * within system container on application start.
     */
    protected const LOAD = [
        // Core Services
        Bootloader\DebugBootloader::class,
        Bootloader\SnapshotsBootloader::class,

        // Security and validation
        Bootloader\Security\EncrypterBootloader::class,
        Bootloader\Security\FiltersBootloader::class,
        Bootloader\Security\GuardBootloader::class,

        RoadRunnerBridge\HttpBootloader::class,

        // HTTP extensions
        Nyholm\NyholmBootloader::class,
        Bootloader\Http\RouterBootloader::class,
        Bootloader\Http\ErrorHandlerBootloader::class,

        // Databases
        CycleBridge\DatabaseBootloader::class,
        CycleBridge\MigrationsBootloader::class,

        // ORM
        CycleBridge\SchemaBootloader::class,
        CycleBridge\CycleOrmBootloader::class,
        CycleBridge\AnnotatedBootloader::class,
        CycleBridge\CommandBootloader::class,

        // Template engine
        Stempler\StemplerBootloader::class,

        Scaffolder\ScaffolderBootloader::class,

        // Framework commands
        Bootloader\CommandBootloader::class,
        RoadRunnerBridge\CommandBootloader::class,

        RoutesBootloader::class,
    ];
}
