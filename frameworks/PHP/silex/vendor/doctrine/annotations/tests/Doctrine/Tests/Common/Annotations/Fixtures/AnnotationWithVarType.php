<?php

namespace Doctrine\Tests\Common\Annotations\Fixtures;

/**
 * @Annotation
 * @Target("ALL")
 */
final class AnnotationWithVarType
{

    /**
     * @var mixed
     */
    public $mixed;

    /**
     * @var boolean
     */
    public $boolean;

    /**
     * @var bool
     */
    public $bool;

    /**
     * @var float
     */
    public $float;

    /**
     * @var string
     */
    public $string;

    /**
     * @var integer
     */
    public $integer;

    /**
     * @var array
     */
    public $array;

    /**
     * @var Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll
     */
    public $annotation;

    /**
     * @var array<integer>
     */
    public $arrayOfIntegers;

    /**
     * @var array<Doctrine\Tests\Common\Annotations\Fixtures\AnnotationTargetAll>
     */
    public $arrayOfAnnotations;

}