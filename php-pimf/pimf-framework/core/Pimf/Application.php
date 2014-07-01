<?php
/**
 * Pimf
 *
 * @copyright Copyright (c) Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf;

use Pimf\Util\String as Str;

/**
 * Provides a facility for applications which provides reusable resources,
 * common-based bootstrapping and dependency checking.
 *
 * @package Pimf
 * @author  Gjero Krsteski <gjero@krsteski.de>
 *
 */
final class Application
{
  const VERSION = '1.8.6';

  /**
   * Mechanism used to do some initial config before a Application runs.
   *
   * @param array $conf   The array of configuration options.
   * @param array $server Array of information such as headers, paths, and script locations.
   *
   * @return boolean|null
   */
  public static function bootstrap(array $conf, array $server = array())
  {
    $problems = array();

    try {

      ini_set('default_charset', $conf['encoding']);
      date_default_timezone_set($conf['timezone']);

      self::registerLocalEnvironment($conf, $server);
      self::setupErrorHandling($conf);
      self::loadPdoDriver($conf);
      self::loadRoutes($conf['app']['routeable'], BASE_PATH . 'app/' . $conf['app']['name'] . '/routes.php');
      self::loadListeners(BASE_PATH . 'app/' . $conf['app']['name'] . '/events.php');

    } catch (\Exception $exception) {
      $problems[] = $exception->getMessage();
    }

    self::reportIf($problems, PHP_VERSION);
  }

  /**
   * Please bootstrap first, than run the application!
   *
   * Run a application, let application accept a request, route the request,
   * dispatch to controller/action, render response and return response to client finally.
   *
   * @param array $get    Array of variables passed to the current script via the URL parameters.
   * @param array $post   Array of variables passed to the current script via the HTTP POST method.
   * @param array $cookie Array of variables passed to the current script via HTTP Cookies.
   *
   * @throws \LogicException If application not bootstrapped.
   * @return void
   */
  public static function run(array $get, array $post, array $cookie)
  {
    $cli = array();
    if (Sapi::isCli()) {
      $cli = Cli::parse((array)Registry::get('env')->argv);
      if (count($cli) < 1 || isset($cli['list'])) {
        Cli::absorb();
        exit(0);
      }
    }

    $conf       = Registry::get('conf');
    $prefix     = Str::ensureTrailing('\\', $conf['app']['name']);
    $repository = BASE_PATH . 'app/' . $conf['app']['name'] . '/Controller';

    if (isset($cli['controller']) && $cli['controller'] == 'core') {
      $prefix     = 'Pimf\\';
      $repository = BASE_PATH . 'pimf-framework/core/Pimf/Controller';
    }

    $resolver = new Resolver(new Request($get, $post, $cookie, $cli), $repository, $prefix);

    $sessionized = (Sapi::isWeb() && $conf['session']['storage'] !== '');

    if ($sessionized) {
      Session::load();
    }

    $pimf = $resolver->process();

    if ($sessionized) {
      Session::save();
      Cookie::send();
    }

    $pimf->render();
  }

  /**
   * @param array $conf
   * @param array $server
   */
  private static function registerLocalEnvironment(array $conf, array $server)
  {
    Registry::set('conf', $conf);
    Registry::set('env', new Environment($server));
    Registry::set('logger', new Logger($conf['bootstrap']['local_temp_directory']));

    Registry::get('logger')->init();
  }

  /**
   * @param array $conf
   */
  private static function setupErrorHandling(array $conf)
  {
    if ($conf['environment'] == 'testing') {
      error_reporting(E_ALL | E_STRICT);
    } else {

      set_exception_handler(
        function ($exception) {
          Error::exception($exception);
        }
      );

      set_error_handler(
        function ($code, $error, $file, $line) {
          Error::native($code, $error, $file, $line);
        }
      );

      register_shutdown_function(
        function () {
          Error::shutdown();
        }
      );

      error_reporting(-1);
    }
  }

  /**
   * @param array $conf
   */
  private static function loadPdoDriver(array $conf)
  {
    $dbConf = $conf[$conf['environment']]['db'];

    if (is_array($dbConf) && $conf['environment'] != 'testing') {
      Registry::set('em', new EntityManager(Pdo\Factory::get($dbConf), $conf['app']['name']));
    }
  }

  /**
   * @param boolean $routeable
   * @param string  $routes Path to routes definition file.
   */
  private static function loadRoutes($routeable, $routes)
  {
    if ($routeable === true && file_exists($routes)) {

      Registry::set('router', new Router());

      foreach ((array)(include $routes) as $route) {

        Registry::get('router')->map($route);

      }
    }
  }

  /**
   * @param string $events Path to event listeners
   */
  private static function loadListeners($events)
  {
    if (file_exists($events)) {
      include_once $events;
    }
  }

  /**
   * @param array $problems
   * @param float $version
   * @param bool  $die
   *
   * @return array|void
   */
  private static function reportIf(array $problems, $version, $die = true)
  {
    if (version_compare($version, 5.3) == -1) {
      $problems[] = 'You have PHP ' . $version . ' and you need 5.3 or higher!';
    }

    if (!empty($problems)) {
      return ($die === true) ? die(implode(PHP_EOL . PHP_EOL, $problems)) : $problems;
    }
  }

  /**
   * PIMF Application can not be cloned.
   */
  private function __clone() { }

  /**
   * Stopping the PHP process for PHP-FastCGI users to speed up some PHP queries.
   */
  public static function finish()
  {
    if (function_exists('fastcgi_finish_request')) {
      fastcgi_finish_request();
    }
  }
}
