<?php

namespace Doctrine\Tests\Common\Annotations\Fixtures;

/**
 * @Annotation
 * @Target("ALL")
 * @Attributes({
      @Attribute("value",   required = true ,   type = "string"),
      @Attribute("annot",   required = true ,   type = "Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAnnotation"),
   })
 */
final class AnnotationWithRequiredAttributes
{

    public final function __construct(array $data)
    {
        foreach ($data as $key => $value) {
            $this->$key = $value;
        }
    }

    /**
     * @var string
     */
    private $value;

    /**
     *
     * @var Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAnnotation
     */
    private $annot;

    /**
     * @return string
     */
    public function getValue()
    {
        return $this->value;
    }

    /**
     * @return Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAnnotation
     */
    public function getAnnot()
    {
        return $this->annot;
    }

}