<?php

namespace Config;

use CodeIgniter\Config\BaseConfig;
use CodeIgniter\Debug\Toolbar\Collectors\Database;
use CodeIgniter\Debug\Toolbar\Collectors\Events;
use CodeIgniter\Debug\Toolbar\Collectors\Files;
use CodeIgniter\Debug\Toolbar\Collectors\Logs;
use CodeIgniter\Debug\Toolbar\Collectors\Routes;
use CodeIgniter\Debug\Toolbar\Collectors\Timers;
use CodeIgniter\Debug\Toolbar\Collectors\Views;

/**
 * --------------------------------------------------------------------------
 * Debug Toolbar
 * --------------------------------------------------------------------------
 *
 * The Debug Toolbar provides a way to see information about the performance
 * and state of your application during that page display. By default it will
 * NOT be displayed under production environments, and will only display if
 * `CI_DEBUG` is true, since if it's not, there's not much to display anyway.
 */
class Toolbar extends BaseConfig
{
    /**
     * --------------------------------------------------------------------------
     * Toolbar Collectors
     * --------------------------------------------------------------------------
     *
     * List of toolbar collectors that will be called when Debug Toolbar
     * fires up and collects data from.
     *
     * @var string[]
     */
    public $collectors = [
        Timers::class,
        Database::class,
        Logs::class,
        Views::class,
        // \CodeIgniter\Debug\Toolbar\Collectors\Cache::class,
        Files::class,
        Routes::class,
        Events::class,
    ];

    /**
     * --------------------------------------------------------------------------
     * Collect Var Data
     * --------------------------------------------------------------------------
     *
     * If set to false var data from the views will not be colleted. Usefull to
     * avoid high memory usage when there are lots of data passed to the view.
     *
     * @var bool
     */
    public $collectVarData = true;

    /**
     * --------------------------------------------------------------------------
     * Max History
     * --------------------------------------------------------------------------
     *
     * `$maxHistory` sets a limit on the number of past requests that are stored,
     * helping to conserve file space used to store them. You can set it to
     * 0 (zero) to not have any history stored, or -1 for unlimited history.
     *
     * @var int
     */
    public $maxHistory = 20;

    /**
     * --------------------------------------------------------------------------
     * Toolbar Views Path
     * --------------------------------------------------------------------------
     *
     * The full path to the the views that are used by the toolbar.
     * This MUST have a trailing slash.
     *
     * @var string
     */
    public $viewsPath = SYSTEMPATH . 'Debug/Toolbar/Views/';

    /**
     * --------------------------------------------------------------------------
     * Max Queries
     * --------------------------------------------------------------------------
     *
     * If the Database Collector is enabled, it will log every query that the
     * the system generates so they can be displayed on the toolbar's timeline
     * and in the query log. This can lead to memory issues in some instances
     * with hundreds of queries.
     *
     * `$maxQueries` defines the maximum amount of queries that will be stored.
     *
     * @var int
     */
    public $maxQueries = 100;
}
