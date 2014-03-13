<?php
/**
 * Pimf
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf;

/**
 * Provides a great way to build de-coupled applications and allows plug-ins to tap
 * into the core of your application without modifying the code.
 *
 * Register a callback for a given event.
 *
 * <code>
 *    // register a callback for the "start" event
 *    Efs_Event::listen('start', function () {return 'Started!';});
 *
 *    // register an object instance callback for the given event
 *    Efs_Event::listen('event', array($object, 'method'));
 * </code>
 *
 * Fire an event and return the first response.
 *
 * <code>
 *    // fire the "start" event
 *    $response = Efs_Event::first('start');
 *
 *    // fire the "start" event passing an array of parameters
 *    $response = Efs_Event::first('start', array('Pimf', 'Framework'));
 * </code>
 *
 * Fire an event so that all listeners are called.
 *
 * <code>
 *    // fire the "start" event
 *    $responses = Efs_Event::fire('start');
 *
 *    // fire the "start" event passing an array of parameters
 *    $responses = Efs_Event::fire('start', array('Pimf', 'Framework'));
 *
 *    // fire multiple events with the same parameters
 *    $responses = Efs_Event::fire(array('start', 'loading'), $parameters);
 * </code>
 *
 * @package Pimf
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Event
{
  /**
   * All registered events.
   *
   * @var array
   */
  protected static $events = array();

  /**
   * Determine if an event has any registered listeners.
   *
   * @param string $event
   *
   * @return bool
   */
  public static function listeners($event)
  {
    return isset(static::$events[$event]);
  }

  /**
   * Register a callback for a given event.
   *
   * @param string $event
   * @param mixed  $callback
   *
   * @return void
   */
  public static function listen($event, $callback)
  {
    static::$events[$event][] = $callback;
  }

  /**
   * Override all callbacks for a given event with a new callback.
   *
   * @param string $event
   * @param mixed  $callback
   *
   * @return void
   */
  public static function override($event, $callback)
  {
    static::clear($event);
    static::listen($event, $callback);
  }

  /**
   * Clear all event listeners for a given event.
   *
   * @param string $event
   *
   * @return void
   */
  public static function clear($event)
  {
    unset(static::$events[$event]);
  }

  /**
   * Fire an event and return the first response.
   *
   * @param string $event
   * @param \Exception[]  $parameters
   *
   * @return mixed
   */
  public static function first($event, $parameters = array())
  {
    $responses = static::fire($event, $parameters);

    return reset($responses);
  }

  /**
   * Fire an event and return the first response.
   * Execution will be halted after the first valid response is found.
   *
   * @param string $event
   * @param array  $parameters
   *
   * @return mixed
   */
  public static function until($event, $parameters = array())
  {
    return static::fire($event, $parameters, true);
  }

  /**
   * Fire an event so that all listeners are called.
   *
   * @param string $events
   * @param array        $parameters
   * @param bool         $halt
   *
   * @return array|null
   */
  public static function fire($events, $parameters = array(), $halt = false)
  {
    $responses = array();

    $parameters = (array)$parameters;

    // If the event has listeners, iterate through them and call each listener,
    // passing in the parameters.
    foreach ((array)$events as $event) {

      if (static::listeners($event)) {

        foreach (static::$events[$event] as $callback) {
          $response = call_user_func_array($callback, $parameters);

          // If the event is set to halt,
          // return the first response that is not null.
          if ($halt and !is_null($response)) {
            return $response;
          }

          $responses[] = $response;
        }
      }
    }

    return $halt ? null : $responses;
  }
}
