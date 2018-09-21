<?php

namespace Benchmark\Entities;

use Hamlet\Entities\AbstractTwigEntity;

class FortuneEntity extends AbstractTwigEntity
{
    private $messages;

    public function __construct(array $messages)
    {
        $this->messages = $messages;
    }

    protected function getTemplateData()
    {
        return [
            'messages' => $this->messages
        ];
    }

    protected function getTemplatePath(): string
    {
        return __DIR__ . '/fortune.twig';
    }

    /**
     * Get cache key of the entity
     */
    public function getKey(): string
    {
        return md5(var_export($this->messages, true));
    }
}
