<?php
/**
 * Controller
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Controller;

use \Pimf\Param, \Pimf\Registry, \Pimf\Sapi, \Pimf\Controller\Exception as Bomb, \Pimf\Request, \Pimf\Util\Header, \Pimf\Url,
  \Pimf\Response;

/**
 * Defines the general controller behaviour - you have to extend it.
 *
 * @package Controller
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
abstract class Base
{
  /**
   * @var \Pimf\Request
   */
  protected $request;

  /**
   * @var \Pimf\Response
   */
  protected $response;

  /**
   * @param Request  $request
   * @param Response $response
   */
  public function __construct(\Pimf\Request $request, \Pimf\Response $response = null)
  {
    $this->request  = $request;
    $this->response = $response;
  }

  abstract public function indexAction();

  /**
   * Method to show the content.
   *
   * @return mixed
   * @throws \Exception If not supported request method or bad controller
   */
  public function render()
  {
    $conf = Registry::get('conf');

    if (Sapi::isCli() && $conf['environment'] == 'production') {

      $suffix = 'CliAction';
      $action = $this->request->fromCli()->get('action') ? : 'index';

    } else {

      $requestMethod = ucfirst(Registry::get('env')->REQUEST_METHOD);
      $suffix        = 'Action';

      if (!method_exists($this->request, $bag = 'from' . $requestMethod)) {
        throw new Bomb("not supported request method=" . $requestMethod);
      }

      $action = $this->request->{$bag}()->get('action') ? : 'index';

      if ($conf['app']['routeable'] === true) {

        $target = Registry::get('router')->find();

        if ($target instanceof \Pimf\Route\Target) {

          $action = $target->getAction();

          Request::$getData = new Param((array)Request::stripSlashesIfMagicQuotes(
            array_merge($target->getParams(), Request::$getData->getAll())
          ));
        }
      }
    }

    $action = strtolower($action) . $suffix;

    if (method_exists($this, 'init')) {
      call_user_func(array($this, 'init'));
    }

    if (!method_exists($this, $action)) {
      throw new Bomb("no action '{$action}' defined at controller " . get_class($this));
    }

    return call_user_func(array($this, $action));
  }

  /**
   * Prepares the response object to return an HTTP Redirect response to the client.
   *
   * @param string  $route     The redirect destination like controller/action
   * @param boolean $permanent If permanent redirection or not.
   * @param boolean $exit
   */
  public function redirect($route, $permanent = false, $exit = true)
  {
    $url = Url::compute($route);

    Header::clear();

    ($permanent === true) ? Header::sendMovedPermanently() : Header::sendFound();

    Header::toLocation($url, $exit);
  }
}
