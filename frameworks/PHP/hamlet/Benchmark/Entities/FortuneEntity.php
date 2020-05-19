<?php

namespace Benchmark\Entities;

use Hamlet\Http\Entities\AbstractMustacheEntity;

class FortuneEntity extends AbstractMustacheEntity
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
        return __DIR__ . '/fortune.mustache';
    }

    public function getKey(): string
    {
        return md5(var_export($this->messages, true));
    }
}
