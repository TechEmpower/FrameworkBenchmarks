<?php

/**
 * This file is part of CodeIgniter 4 framework.
 *
 * (c) CodeIgniter Foundation <admin@codeigniter.com>
 *
 * For the full copyright and license information, please view
 * the LICENSE file that was distributed with this source code.
 */

namespace Config;

use CodeIgniter\Config\Routing as BaseRouting;

/**
 * Routing configuration
 */
class Routing extends BaseRouting
{
    /**
     * For Defined Routes.
     * An array of files that contain route definitions.
     * Route files are read in order, with the first match
     * found taking precedence.
     *
     * Default: APPPATH . 'Config/Routes.php'
     *
     * @var list<string>
     */
    public array $routeFiles = [
        APPPATH . 'Config/Routes.php',
    ];

    /**
     * For Defined Routes and Auto Routing.
     * The default namespace to use for Controllers when no other
     * namespace has been specified.
     *
     * Default: 'App\Controllers'
     */
    public string $defaultNamespace = 'App\Controllers';

    /**
     * For Auto Routing.
     * The default controller to use when no other controller has been
     * specified.
     *
     * Default: 'Home'
     */
    public string $defaultController = 'Home';

    /**
     * For Defined Routes and Auto Routing.
     * The default method to call on the controller when no other
     * method has been set in the route.
     *
     * Default: 'index'
     */
    public string $defaultMethod = 'index';

    /**
     * For Auto Routing.
     * Whether to translate dashes in URIs for controller/method to underscores.
     * Primarily useful when using the auto-routing.
     *
     * Default: false
     */
    public bool $translateURIDashes = false;

    /**
     * Sets the class/method that should be called if routing doesn't
     * find a match. It can be the controller/method name like: Users::index
     *
     * This setting is passed to the Router class and handled there.
     *
     * If you want to use a closure, you will have to set it in the
     * routes file by calling:
     *
     * $routes->set404Override(function() {
     *    // Do something here
     * });
     *
     * Example:
     *  public $override404 = 'App\Errors::show404';
     */
    public ?string $override404 = null;

    /**
     * If TRUE, the system will attempt to match the URI against
     * Controllers by matching each segment against folders/files
     * in APPPATH/Controllers, when a match wasn't found against
     * defined routes.
     *
     * If FALSE, will stop searching and do NO automatic routing.
     */
    public bool $autoRoute = false;

    /**
     * For Defined Routes.
     * If TRUE, will enable the use of the 'prioritize' option
     * when defining routes.
     *
     * Default: false
     */
    public bool $prioritize = false;

    /**
     * For Defined Routes.
     * If TRUE, matched multiple URI segments will be passed as one parameter.
     *
     * Default: false
     */
    public bool $multipleSegmentsOneParam = false;

    /**
     * For Auto Routing (Improved).
     * Map of URI segments and namespaces.
     *
     * The key is the first URI segment. The value is the controller namespace.
     * E.g.,
     *   [
     *       'blog' => 'Acme\Blog\Controllers',
     *   ]
     *
     * @var array<string, string>
     */
    public array $moduleRoutes = [];

    /**
     * For Auto Routing (Improved).
     * Whether to translate dashes in URIs for controller/method to CamelCase.
     * E.g., blog-controller -> BlogController
     *
     * If you enable this, $translateURIDashes is ignored.
     *
     * Default: false
     */
    public bool $translateUriToCamelCase = true;
}
