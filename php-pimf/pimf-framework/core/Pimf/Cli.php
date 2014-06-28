<?php
/**
 * Pimf
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf;

use Pimf\Util\String;

/**
 * A full featured package for managing command-line options and arguments,
 * it allows the developer to easily build complex command line interfaces.
 *
 * @package Pimf
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
final class Cli
{
  /**
   * Prints out a list of CLI commands from the system,
   * which is defined at the controllers with the "CliAction()" suffix at the method-name.
   *
   * @param string $appClr  Path to application controller repository
   * @param string $coreClr Path to core controller repository
   * @param string $root    Path to home directory
   */
  public static function absorb($appClr = null, $coreClr = null, $root = null)
  {
    echo PHP_EOL . 'PIMF v' . \Pimf\Application::VERSION . ' PHP Command Line Interface by Gjero Krsteski' . PHP_EOL;

    echo '+------------------------------------------------------+' . PHP_EOL;

    self::reflect(self::collect($appClr, $coreClr, $root));
  }

  /**
   * @param array $classes
   */
  public static function reflect(array $classes)
  {
    array_map(
      function ($class) {

        $reflection = new \ReflectionClass($class);

        if ($reflection->isSubclassOf('\Pimf\Controller\Base')) {

          $methods    = $reflection->getMethods();
          $controller = explode('_', $class);

          echo 'controller: ' . strtolower(end($controller)) . '' . PHP_EOL;

          array_map(
            function ($method) {
              if (false !== $command = strstr($method->getName(), 'CliAction', true)) {
                echo PHP_EOL . ' action: ' . $command . ' ' . PHP_EOL;
              }
            }, $methods
          );

          echo PHP_EOL . '+------------------------------------------------------+' . PHP_EOL;
        }

      }, $classes
    );
  }

  /**
   * @param string $appClr
   * @param string $coreClr
   * @param string $root
   *
   * @return array
   */
  public static function collect($appClr = null, $coreClr = null, $root = null)
  {
    $classes = array();
    $conf    = Registry::get('conf');
    $dis     = DIRECTORY_SEPARATOR;

    if (!$root && !$coreClr && !$appClr) {
      // compute the PIMF framework path restriction.
      $root    = dirname(dirname(dirname(dirname(__FILE__))));
      $coreClr = str_replace('/', $dis, $root . '/pimf-framework/core/Pimf/Controller/');
      $appClr  = str_replace('/', $dis, $root . '/app/' . $conf['app']['name'] . '/Controller/');
    }

    foreach (array($appClr, $coreClr) as $dir) {

      $iterator
        = new \RegexIterator(new \RecursiveIteratorIterator(new \RecursiveDirectoryIterator($dir)), '/^.+\.php$/i', \RecursiveRegexIterator::GET_MATCH);

      foreach (iterator_to_array($iterator, false) as $file) {
        $file = str_replace("\\", '/', current($file));
        $file = str_replace('/', $dis, $file);
        $name = str_replace(
          array($root . $dis . 'pimf-framework' . $dis . 'core' . $dis, $root . $dis . 'app' . $dis), '', $file
        );

        $name      = str_replace($dis, '\\', $name);
        $name      = str_replace('.php', '', $name);
        $classes[] = '\\' . $name;
      }
    }

    return $classes;
  }

  /**
   * @param array $commands
   *
   * @return array
   */
  public static function parse(array $commands)
  {
    $cli = array();

    parse_str(implode('&', array_slice($commands, 1)), $cli);

    $command = current(array_keys((array)$cli, ''));

    if (String::contains($command, ':')) {

      list($controller, $action) = explode(':', $command);

      $cli['controller'] = $controller;
      $cli['action']     = $action;
    }

    return $cli;
  }
}
