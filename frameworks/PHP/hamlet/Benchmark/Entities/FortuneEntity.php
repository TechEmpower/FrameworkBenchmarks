<?php

namespace Benchmark\Entities;

use Hamlet\Http\Entities\AbstractMustacheEntity;

class FortuneEntity extends AbstractMustacheEntity
{
    public function __construct(private array $messages) {}

    protected function getTemplateData()
    {
        return [
            'messages' => $this->messages
        ];
    }

    protected function getTemplatePath(): string
    {
        return __DIR__ . '/fortune.mustache';
    }

    public function getKey(): string
    {
        return md5(var_export($this->messages, true));
    }
}
