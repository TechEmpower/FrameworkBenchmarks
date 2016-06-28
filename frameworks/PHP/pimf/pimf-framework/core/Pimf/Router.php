<?php
/**
 * Pimf
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf;

use Pimf\Route\Target;

/**
 * Router
 *
 * This class is responsible for registering route objects, assigning names to route objects,
 * finding routes that match the current HTTP request, and creating URLs for a named route.
 *
 * @package Pimf
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Router
{
  /**
   * @var Route[]
   */
  protected $routes = array();

  public function __construct()
  {
    //it is a pimf-framework restriction.
    $this->map(new Route('/:controller'))->map(new Route('/:controller/:action'))->map(new Route('/:controller/:action/:id'));
  }

  /**
   * @param Route $route
   *
   * @return Router
   */
  public function map(Route $route)
  {
    $this->routes[$route->getRule()] = $route;

    return $this;
  }

  /**
   * @param Route $route
   *
   * @return Target
   */
  private function target(Route $route)
  {
    $params = $route->getParams();

    $target = new Target($params['controller']);

    unset($params['controller']);

    if (isset($params['action'])) {
      $target->setAction($params['action']);
      unset($params['action']);
    }

    $target->setParams($params);

    return $target;
  }

  /**
   * @return bool|Target
   */
  public function find()
  {
    // check custom routes first
    // than framework's restriction routes.
    foreach (array_reverse($this->routes) as $route) {
      if ($route->matches() === true) {
        return $this->target($route);
      }
    }

    return false;
  }
}
