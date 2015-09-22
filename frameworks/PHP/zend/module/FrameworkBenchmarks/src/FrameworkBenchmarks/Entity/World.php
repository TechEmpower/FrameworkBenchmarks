<?php

namespace FrameworkBenchmarks\Entity;

use Zend\Stdlib\ArrayObject;

/**
 * Entity that represents a single entry in the `World` collection of the benchmarks in FrameworkBenchmarks
 *
 * @author Marco Pivetta <ocramius@gmail.com>
 * @link   http://www.techempower.com/benchmarks
 */
class World extends ArrayObject
{
    /**
     * @var int
     */
    public $id;

    /**
     * @var int
     */
    public $randomNumber;

    /**
     * @param array $data
     */
    public function exchangeArray($data)
    {
        $this->id           = (int) $data['id'];
        $this->randomNumber = (int) $data['randomNumber'];
    }

    /**
     * @return array
     */
    public function toArray()
    {
        return array('id' => $this->id, 'randomNumber' => $this->randomNumber);
    }
}
