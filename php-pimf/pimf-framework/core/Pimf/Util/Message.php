<?php
/**
 * Util
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf\Util;

/**
 * Responsible for general message formatting, used for message flashing or with combination with the translator.
 *
 * <code>
 * $message = new Message(
 *   'Hello %your_name my name is %my_name! '
 *    .'I am %my_age, how old are you? I like %object!'
 * );
 *
 * $message->bind('your_name', 'Ben')
 *         ->bind('my_name', 'Matt')
 *         ->bind('my_age', '21')
 *         ->bind('object', 'food');
 *
 * print $message;
 *
 * .. or ..
 *
 * $msg = $message->format();
 *
 * .. output will be = "Hello Ben my name is Matt! I am 21, how old are you? I like food!"
 * </code>
 *
 * @package Util
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Message
{
  /**
   * @var string The message.
   */
  protected $message = '';

  /**
   * @var array A list of tokes which should be bind.
   */
  protected $bindings = array();

  /**
   * @var string The prefixed delimiter for the tokens.
   */
  protected $delimiter = '%';

  /**
   * @param string $message  The message or the resource.
   * @param array  $bindings (Optional) A List of tokes whitch should be bind.
   */
  public function __construct($message, array $bindings = array())
  {
    $this->setMessage($message);
    $this->bindings = $bindings;
  }

  /**
   * @return string
   */
  public function getMessage()
  {
    return $this->message;
  }

  /**
   * @param string $message The message.
   *
   * @return Message
   */
  public function setMessage($message)
  {
    $this->message = $message;

    return $this;
  }

  /**
   * @param string $char The character for the prexied delimitation of the tokens.
   *
   * @return Message
   */
  public function setDelimiter($char)
  {
    $this->delimiter = $char;

    return $this;
  }

  /**
   * Sets/Updates the value for the given token.
   *
   * @param string $token The token.
   * @param string $value The value for replacement.
   *
   * @return Message
   */
  public function bind($token, $value)
  {
    $this->bindings[$token] = $value;

    return $this;
  }

  /**
   * If the object is treated as string.
   *
   * @return string
   */
  public function __toString()
  {
    return (string) $this->format();
  }

  /**
   * Retuns formated message.
   *
   * @return string
   */
  public function format()
  {
    if ($this->getMessage() == '' || !$this->getMessage()) {
      return '';
    }

    if (count($this->bindings) > 0) {
      foreach ($this->bindings as $token => $value) {
        $this->message = str_replace($this->delimiter . $token, $value, $this->message);
      }
    }

    return $this->message;
  }
}
