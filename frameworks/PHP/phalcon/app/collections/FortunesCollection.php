<?php

use Phalcon\Incubator\MongoDB\Mvc\Collection;

class FortunesCollection extends Collection
{
    public $message;

    public function onConstruct(): void
    {
        $this->setSource('fortune');
        $this->useImplicitObjectIds(false);
    }
}
