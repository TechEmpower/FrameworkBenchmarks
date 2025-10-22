<?php

namespace Config;

use CodeIgniter\Config\BaseConfig;
use Kint\Parser\ConstructablePluginInterface;
use Kint\Renderer\AbstractRenderer;
use Kint\Renderer\Rich\TabPluginInterface;
use Kint\Renderer\Rich\ValuePluginInterface;

/**
 * --------------------------------------------------------------------------
 * Kint
 * --------------------------------------------------------------------------
 *
 * We use Kint's `RichRenderer` and `CLIRenderer`. This area contains options
 * that you can set to customize how Kint works for you.
 *
 * @see https://kint-php.github.io/kint/ for details on these settings.
 */
class Kint extends BaseConfig
{
    /*
    |--------------------------------------------------------------------------
    | Global Settings
    |--------------------------------------------------------------------------
    */

    /**
     * @var array<int, ConstructablePluginInterface|string>
     * @phpstan-var list<class-string<ConstructablePluginInterface>|ConstructablePluginInterface>
     */
    public $plugins;

    public int $maxDepth           = 6;
    public bool $displayCalledFrom = true;
    public bool $expanded          = false;

    /*
    |--------------------------------------------------------------------------
    | RichRenderer Settings
    |--------------------------------------------------------------------------
    */
    public string $richTheme = 'aante-light.css';
    public bool $richFolder  = false;
    public int $richSort     = AbstractRenderer::SORT_FULL;

    /**
     * @var array<string, string>
     * @phpstan-var array<string, class-string<ValuePluginInterface>>
     */
    public $richObjectPlugins;

    /**
     * @var array<string, string>
     * @phpstan-var array<string, class-string<TabPluginInterface>>
     */
    public $richTabPlugins;

    /*
    |--------------------------------------------------------------------------
    | CLI Settings
    |--------------------------------------------------------------------------
    */
    public bool $cliColors      = true;
    public bool $cliForceUTF8   = false;
    public bool $cliDetectWidth = true;
    public int $cliMinWidth     = 40;
}
