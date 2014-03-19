<?php
/**
 * Pimf
 *
 * @copyright Copyright (c)  Gjero Krsteski (http://krsteski.de)
 * @license   http://krsteski.de/new-bsd-license New BSD License
 */

namespace Pimf;

/**
 * @package Pimf
 * @author  Gjero Krsteski <gjero@krsteski.de>
 */
class Param
{
  /**
   * @var \ArrayObject|null
   */
  protected $data = null;

  /**
   * @param array $data
   */
  public function __construct(array $data = array())
  {
    $this->data = new \ArrayObject($data, \ArrayObject::STD_PROP_LIST + \ArrayObject::ARRAY_AS_PROPS);
  }

  /**
   * @return array
   */
  public function getAll()
  {
    return (array)$this->data->getArrayCopy();
  }

  /**
   * @param string $index
   * @param mixed  $defaultValue
   * @param bool   $filtered If you trust foreign input introduced to your PHP code - set to FALSE!
   *
   * @return string
   */
  public function get($index, $defaultValue = null, $filtered = true)
  {
    if ($this->data->offsetExists($index)) {

      if ($filtered === true) {
        // pretty high-level filtering here...
        return self::filter($this->data->offsetGet($index));
      }

      return $this->data->offsetGet($index);
    }

    return $defaultValue;
  }

  /**
   * Never ever (ever) trust foreign input introduced to your PHP code!
   *
   * @param array|string $rawData
   *
   * @return array|bool|string
   */
  public static function filter($rawData)
  {
    return is_array($rawData)

      ? array_map(
        function ($value) {
          return \Pimf\Util\String\Clean::xss($value);
        }, $rawData
      )

      : \Pimf\Util\String\Clean::xss($rawData);
  }
}
