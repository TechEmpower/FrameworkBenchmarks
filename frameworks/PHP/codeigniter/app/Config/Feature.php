<?php

namespace Config;

use CodeIgniter\Config\BaseConfig;

/**
 * Enable/disable backward compatibility breaking features.
 */
class Feature extends BaseConfig
{
    /**
     * Enable multiple filters for a route or not.
     *
     * If you enable this:
     *   - CodeIgniter\CodeIgniter::handleRequest() uses:
     *     - CodeIgniter\Filters\Filters::enableFilters(), instead of enableFilter()
     *   - CodeIgniter\CodeIgniter::tryToRouteIt() uses:
     *     - CodeIgniter\Router\Router::getFilters(), instead of getFilter()
     *   - CodeIgniter\Router\Router::handle() uses:
     *     - property $filtersInfo, instead of $filterInfo
     *     - CodeIgniter\Router\RouteCollection::getFiltersForRoute(), instead of getFilterForRoute()
     */
    public bool $multipleFilters = false;

    /**
     * Use improved new auto routing instead of the default legacy version.
     */
    public bool $autoRoutesImproved = false;
}
