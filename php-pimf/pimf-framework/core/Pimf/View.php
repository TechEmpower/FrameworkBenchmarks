<?php
/**
 * Pimf
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf;

use Pimf\Contracts\Renderable;
use Pimf\Util\File;
use Pimf\Contracts\Arrayable;

/**
 * A simply view for sending and rendering data.
 *
 * @package Pimf
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class View implements Renderable
{
  /**
   * @var string Name of the template.
   */
  protected $template;

  /**
   * Contains the variables that are to be embedded in the template.
   *
   * @var \ArrayObject
   */
  protected $data;

  /**
   * Path to templates - is framework restriction.
   *
   * @var string
   */
  protected $path;

  /**
   * @param string $template
   * @param array  $data
   * @param string $path Path to templates if you do not want to use PIMF framework restriction.
   */
  public function __construct($template = 'default.phtml', array $data = array(), $path = null)
  {
    $conf           = Registry::get('conf');
    $this->data     = new \ArrayObject($data, \ArrayObject::ARRAY_AS_PROPS);
    $this->path     = (!$path) ? BASE_PATH . 'app/' . $conf['app']['name'] . '/_templates' : $path;
    $this->template = (string)$template;
  }

  /**
   * @param string $template
   *
   * @return View
   */
  public function produce($template)
  {
    $view           = clone $this;
    $view->template = (string)$template;

    return $view;
  }

  /**
   * @param string          $template
   * @param array|Arrayable $model
   *
   * @return string
   */
  public function partial($template, $model = array())
  {
    $model = ($model instanceof Arrayable) ? $model->toArray() : $model;

    return $this->produce($template)->pump($model)->render();
  }

  /**
   * @param string $template
   * @param array  $model
   *
   * @return string
   */
  public function loop($template, array $model = array())
  {
    $out = '';

    foreach ($model as $row) {
      $out .= $this->partial($template, $row);
    }

    return $out;
  }

  /**
   * Assigns a variable to a specific key for the template.
   *
   * @param string $key   The key.
   * @param mixed  $value The Value.
   *
   * @return View
   */
  public function assign($key, $value)
  {
    $this->data[$key] = $value;

    return $this;
  }

  /**
   * Exchange all variables.
   *
   * @param $model
   *
   * @return View
   */
  public function pump(array $model)
  {
    $this->data->exchangeArray($model);

    return $this;
  }

  /**
   * @param string $name
   *
   * @return mixed
   * @throws \OutOfBoundsException If undefined property at the template.
   */
  public function __get($name)
  {
    if ($this->data->offsetExists($name)) {
      return $this->data->offsetGet($name);
    }

    $trace = debug_backtrace();
    throw new \OutOfBoundsException(
      'undefined property "' . $name . '" at file ' . $trace[0]['file'] . ' line ' . $trace[0]['line']
    );
  }

  /**
   * @return string
   * @throws \Exception
   */
  public function render()
  {
    $level = ob_get_level();
    ob_start();

    try {

      echo $this->reunite();

    } catch (\Exception $exception) {

      while (ob_get_level() > $level) {
        ob_end_clean();
      }

      throw $exception;
    }

    return ob_get_clean();
  }

  /**
   * Puts the template an the variables together.
   */
  public function reunite()
  {
    include new File(str_replace('/', DS, $this->path . '/' . $this->template));
  }

  /**
   * Act when the view is treated like a string
   *
   * @return string
   */
  public function __toString()
  {
    return $this->render();
  }
}
