<?php

namespace Doctrine\Tests\Common\Annotations\Fixtures;

/**
 * @Annotation
 * @Target("ALL")
 * @Attributes({
      @Attribute("mixed",                type = "mixed"),
      @Attribute("boolean",              type = "boolean"),
      @Attribute("bool",                 type = "bool"),
      @Attribute("float",                type = "float"),
      @Attribute("string",               type = "string"),
      @Attribute("integer",              type = "integer"),
      @Attribute("array",                type = "array"),
      @Attribute("arrayOfIntegers",      type = "array<integer>"),
      @Attribute("annotation",           type = "Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll"),
      @Attribute("arrayOfAnnotations",   type = "array<Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll>"),
  })
 */
final class AnnotationWithAttributes
{

    public final function __construct(array $data)
    {
        foreach ($data as $key => $value) {
            $this->$key = $value;
        }
    }

    private $mixed;
    private $boolean;
    private $bool;
    private $float;
    private $string;
    private $integer;
    private $array;
    private $annotation;
    private $arrayOfIntegers;
    private $arrayOfAnnotations;

    /**
     * @return mixed
     */
    public function getMixed()
    {
        return $this->mixed;
    }

    /**
     * @return boolean
     */
    public function getBoolean()
    {
        return $this->boolean;
    }

    /**
     * @return bool
     */
    public function getBool()
    {
        return $this->bool;
    }

    /**
     * @return float
     */
    public function getFloat()
    {
        return $this->float;
    }

    /**
     * @return string
     */
    public function getString()
    {
        return $this->string;
    }

    public function getInteger()
    {
        return $this->integer;
    }

    /**
     * @return array
     */
    public function getArray()
    {
        return $this->array;
    }

    /**
     * @return Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll
     */
    public function getAnnotation()
    {
        return $this->annotation;
    }

    /**
     * @return array<integer>
     */
    public function getArrayOfIntegers()
    {
        return $this->arrayOfIntegers;
    }

    /**
     * @return array<Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll>
     */
    public function getArrayOfAnnotations()
    {
        return $this->arrayOfAnnotations;
    }

}