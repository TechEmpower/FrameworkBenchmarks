<?php
/**
 * Pimf
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf;

/**
 * Route
 *
 * This class is a relationship of HTTP method(s), an HTTP URI to create
 * a Pimf application route. The Pimf application will determine
 * the one Route object to dispatch for the current HTTP request.
 *
 * Each route object will have a URI pattern. This pattern must match the
 * current HTTP request's URI for the route object to be dispatched by
 * the Pimf application. The route pattern may contain parameters, segments
 * prefixed with a colon (:). For example:
 *
 *     /controller/:action/:id
 *
 * When the route is dispatched, it's parameters array will be populated
 * with the values of the corresponding HTTP request URI segments.
 *
 * @package Pimf
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Route
{
  /**
   * @var bool
   */
  private $matched = true;

  /**
   * @var array
   */
  private $params = array();

  /**
   * The route pattern (e.g. "/controller/:action/:id")
   *
   * @var string
   */
  private $rule;

  /**
   * Array of URL parameter names
   *
   * @var array
   */
  protected $names = array();

  /**
   * Array of URL parameter names with + at the end
   *
   * @var array
   */
  protected $namesPath = array();

  /**
   * Conditions for this route's URL parameters
   *
   * @var array
   */
  private $conditions;

  /**
   * @param string $rule
   * @param array  $target
   * @param array  $conditions
   */
  public function __construct($rule, array $target = array(), array $conditions = array())
  {
    $this->rule       = $rule;
    $this->conditions = $conditions;

    //convert URL params into regex patterns, construct a regex for this route, init params
    $regex = preg_replace_callback(
      '#:([\w]+)\+?#', array($this, 'computeUrlRegex'), str_replace(')', ')?', (string)$rule)
    );

    if (substr($rule, -1) === '/') {
      $regex .= '?';
    }

    //cache URL params' names and values if this route matches the current HTTP request
    $params = array();
    if (!preg_match('#^' . $regex . '$#', self::computeUri(), $params)) {
      $this->matched = false;

      return;
    }

    foreach ($this->names as $name) {
      if (isset($params[$name])) {
        if (isset($this->namesPath[$name])) {
          $this->params[$name] = explode('/', urldecode($params[$name]));
        } else {
          $this->params[$name] = urldecode($params[$name]);
        }
      }
    }

    foreach ($target as $key => $value) {
      $this->params[$key] = $value;
    }
  }

  /**
   * @param array $matches
   *
   * @return string
   */
  private function computeUrlRegex(array $matches)
  {
    $this->names[] = $matches[1];

    if (isset($this->conditions[$matches[1]])) {
      return '(?P<' . $matches[1] . '>' . $this->conditions[$matches[1]] . ')';
    }

    if (substr($matches[0], -1) === '+') {

      $this->namesPath[$matches[1]] = 1;

      return '(?P<' . $matches[1] . '>.+)';
    }

    return '(?P<' . $matches[1] . '>[^/]+)';
  }

  /**
   * @return string
   */
  private function computeUri()
  {
    $uri = Registry::get('env')->REQUEST_URI;
    $pos = strpos($uri, '?');

    if ($pos !== false) {
      $uri = substr($uri, 0, $pos);
    }

    return $uri;
  }

  /**
   * @return boolean
   */
  public function matches()
  {
    return $this->matched;
  }

  /**
   * @return array
   */
  public function getParams()
  {
    return $this->params;
  }

  /**
   * @return string
   */
  public function getRule()
  {
    return $this->rule;
  }
}
