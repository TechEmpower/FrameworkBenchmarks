<?php

namespace Benchmark\Entities;

use Hamlet\Entities\AbstractEntity;

class PlainTextEntity extends AbstractEntity
{
    private $content;

    public function __construct(string $content)
    {
        $this->content = $content;
    }

    public function getContent(): string
    {
        return $this->content;
    }

    /**
     * Get cache key of the entity
     */
    public function getKey(): string
    {
        return md5($this->content);
    }

    /**
     * Get media type
     * @return string|null
     */
    public function getMediaType()
    {
        return 'text/plain';
    }
}
