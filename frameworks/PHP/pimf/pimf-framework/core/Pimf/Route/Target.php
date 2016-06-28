<?php
/**
 * Pimf
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Route;

/**
 * Target
 *
 * The route-target class defines which prefixes get imported
 * and exported on the Pimf request-resolver and controller.
 *
 * @package Route
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Target
{
  /**
   * Controller name as it appears in url
   *
   * @var string
   */
  protected $controller;

  /**
   * Controller action name as it appears in url
   *
   * @var string
   */
  protected $action = 'index';

  /**
   * List of additional params at teh URL.
   *
   * @var array
   */
  protected $params = array();

  /**
   * @param string $controller
   */
  public function __construct($controller)
  {
    $this->controller = $controller;
  }

  /**
   * @param mixed $action
   */
  public function setAction($action)
  {
    $this->action = $action;
  }

  /**
   * @param array $params
   */
  public function setParams(array $params)
  {
    $this->params = $params;
  }

  /**
   * @return string
   */
  public function getAction()
  {
    return $this->action;
  }

  /**
   * Controller-name as it appears in url.
   *
   * @return string
   */
  public function getController()
  {
    return $this->controller;
  }

  /**
   * @return array
   */
  public function getParams()
  {
    return $this->params;
  }
}
