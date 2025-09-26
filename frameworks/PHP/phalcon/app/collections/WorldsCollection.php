<?php

use Phalcon\Incubator\MongoDB\Mvc\Collection;

class WorldsCollection extends Collection
{
    public $randomNumber;

    public function onConstruct(): void
    {
        $this->setSource('world');
        $this->useImplicitObjectIds(false);
    }
}
