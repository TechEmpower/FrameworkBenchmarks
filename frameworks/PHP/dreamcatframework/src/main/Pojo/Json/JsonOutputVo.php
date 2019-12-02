<?php

namespace DreamCat\Benchmark\Pojo\Json;

/**
 * json输出用的vo
 * @author vijay
 */
class JsonOutputVo
{
    /** @var string */
    private $message;

    /**
     * @return string
     */
    public function getMessage(): string
    {
        return $this->message;
    }

    /**
     * @param string $message
     * @return static self
     */
    public function setMessage(string $message): JsonOutputVo
    {
        $this->message = $message;
        return $this;
    }


}

# end of file
