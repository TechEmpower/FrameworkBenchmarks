<?php
/**
 * Pimf
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf;

use Pimf\Util\Header;

/**
 * Defines the default exception handler if an exception is not caught within a try/catch block.
 * Execution will stop after the exception_handler is called.
 *
 * @package Pimf
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Error
{
  /**
   * Handle an exception and display the exception report.
   *
   * @param \Exception $exception
   * @param boolean    $exit
   */
  public static function exception(\Exception $exception, $exit = true)
  {
    static::log($exception);

    ob_get_length() > 0 and ob_get_level() and ob_end_clean();

    $conf = Registry::get('conf');

    if (isset($conf['error']['debug_info']) && $conf['error']['debug_info'] === true) {
      echo static::format($exception);
      if ($exit) {
        exit;
      }
    }

    Header::clear();

    if ($exception instanceof \Pimf\Controller\Exception
      || $exception instanceof \Pimf\Resolver\Exception
    ) {
      Event::first('404', array($exception));
      Header::sendNotFound(null, $exit);
    } else {
      Event::first('500', array($exception));
      Header::sendInternalServerError(null, $exit);
    }
  }

  /**
   * If detailed errors are enabled, just format the exception into
   * a simple error message and display it.
   *
   * @param \Exception $exception
   *
   * @return string
   */
  public static function format(\Exception $exception)
  {
    if (Sapi::isCli()) {
      return
        "+++ Untreated Exception +++" . PHP_EOL . "Message: " . $exception->getMessage() . PHP_EOL . "Location: " . $exception->getFile()
        . " on line " . $exception->getLine() . PHP_EOL . "Stack Trace: " . PHP_EOL . $exception->getTraceAsString() . PHP_EOL;
    }

    return "<html><h2>Untreated Exception</h2>
      <h3>Message:</h3>
      <pre>" . $exception->getMessage() . "</pre>
      <h3>Location:</h3>
      <pre>" . $exception->getFile() . " on line " . $exception->getLine() . "</pre>
      <h3>Stack Trace:</h3>
      <pre>" . $exception->getTraceAsString() . "</pre></html>";
  }

  /**
   * Handle a native PHP error as an ErrorException.
   *
   * @param int    $code
   * @param string $error
   * @param string $file
   * @param int    $line
   */
  public static function native($code, $error, $file, $line)
  {
    if (error_reporting() === 0) {
      return;
    }

    // create an ErrorException for the PHP error
    $exception = new \ErrorException($error, $code, 0, $file, $line);

    $conf = Registry::get('conf');

    if (in_array($code, (array)$conf['error']['ignore_levels'])) {
      return static::log($exception);
    }

    // display the ErrorException
    static::exception($exception);
  }

  /**
   * Handle the PHP shutdown event.
   */
  public static function shutdown()
  {
    // if a fatal error occurred
    $error = error_get_last();

    if (!is_null($error)) {
      static::exception(new \ErrorException($error['message'], $error['type'], 0, $error['file'], $error['line']));
    }
  }

  /**
   * Log an exception.
   *
   * @param \Exception $exception
   */
  public static function log(\Exception $exception)
  {
    $conf = Registry::get('conf');

    if (isset($conf['error']['log']) && $conf['error']['log'] === true) {
      Registry::get('logger')->error($exception->getMessage() . ' ' . $exception->getTraceAsString());
    }
  }
}
