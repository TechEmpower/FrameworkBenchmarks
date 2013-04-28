<?php

namespace FrameworkBenchmarks\Entity;

/**
 * Entity that represents a single entry in the `World` collection of the benchmarks in FrameworkBenchmarks
 *
 * @author Marco Pivetta <ocramius@gmail.com>
 * @link   http://www.techempower.com/benchmarks
 */
class World
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
        $this->id           = $data['id'];
        $this->randomNumber = $data['randomNumber'];
    }

    /**
     * @return array
     */
    public function toArray()
    {
        return array('id' => $this->id, 'randomNumber' => $this->randomNumber);
    }
}
