<?php
/*
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * This software consists of voluntary contributions made by many individuals
 * and is licensed under the MIT license. For more information, see
 * <http://www.doctrine-project.org>.
 */

namespace Doctrine\Common\Annotations;

use Closure;
use ReflectionClass;
use Doctrine\Common\Annotations\Annotation\Enum;
use Doctrine\Common\Annotations\Annotation\Target;
use Doctrine\Common\Annotations\Annotation\Attribute;
use Doctrine\Common\Annotations\Annotation\Attributes;

/**
 * A parser for docblock annotations.
 *
 * It is strongly discouraged to change the default annotation parsing process.
 *
 * @author Benjamin Eberlei <kontakt@beberlei.de>
 * @author Guilherme Blanco <guilhermeblanco@hotmail.com>
 * @author Jonathan Wage <jonwage@gmail.com>
 * @author Roman Borschel <roman@code-factory.org>
 * @author Johannes M. Schmitt <schmittjoh@gmail.com>
 * @author Fabio B. Silva <fabio.bat.silva@gmail.com>
 */
final class DocParser
{
    /**
     * An array of all valid tokens for a class name.
     *
     * @var array
     */
    private static $classIdentifiers = array(DocLexer::T_IDENTIFIER, DocLexer::T_TRUE, DocLexer::T_FALSE, DocLexer::T_NULL);

    /**
     * The lexer.
     *
     * @var \Doctrine\Common\Annotations\DocLexer
     */
    private $lexer;

    /**
     * Current target context
     *
     * @var string
     */
    private $target;

    /**
     * Doc Parser used to collect annotation target
     *
     * @var \Doctrine\Common\Annotations\DocParser
     */
    private static $metadataParser;

    /**
     * Flag to control if the current annotation is nested or not.
     *
     * @var boolean
     */
    private $isNestedAnnotation = false;

    /**
     * Hashmap containing all use-statements that are to be used when parsing
     * the given doc block.
     *
     * @var array
     */
    private $imports = array();

    /**
     * This hashmap is used internally to cache results of class_exists()
     * look-ups.
     *
     * @var array
     */
    private $classExists = array();

    /**
     * Whether annotations that have not been imported should be ignored.
     *
     * @var boolean
     */
    private $ignoreNotImportedAnnotations = false;

    /**
     * An array of default namespaces if operating in simple mode.
     *
     * @var array
     */
    private $namespaces = array();

    /**
     * A list with annotations that are not causing exceptions when not resolved to an annotation class.
     *
     * The names must be the raw names as used in the class, not the fully qualified
     * class names.
     *
     * @var array
     */
    private $ignoredAnnotationNames = array();

    /**
     * @var string
     */
    private $context = '';

    /**
     * Hash-map for caching annotation metadata
     * @var array
     */
    private static $annotationMetadata = array(
        'Doctrine\Common\Annotations\Annotation\Target' => array(
            'is_annotation'    => true,
            'has_constructor'  => true,
            'properties'       => array(),
            'targets_literal'  => 'ANNOTATION_CLASS',
            'targets'          => Target::TARGET_CLASS,
            'default_property' => 'value',
            'attribute_types'  => array(
                'value'  => array(
                    'required'  => false,
                    'type'      =>'array',
                    'array_type'=>'string',
                    'value'     =>'array<string>'
                )
             ),
        ),
        'Doctrine\Common\Annotations\Annotation\Attribute' => array(
            'is_annotation'    => true,
            'has_constructor'  => false,
            'targets_literal'  => 'ANNOTATION_ANNOTATION',
            'targets'          => Target::TARGET_ANNOTATION,
            'default_property' => 'name',
            'properties'       => array(
                'name'      => 'name',
                'type'      => 'type',
                'required'  => 'required'
            ),
            'attribute_types'  => array(
                'value'  => array(
                    'required'  => true,
                    'type'      =>'string',
                    'value'     =>'string'
                ),
                'type'  => array(
                    'required'  =>true,
                    'type'      =>'string',
                    'value'     =>'string'
                ),
                'required'  => array(
                    'required'  =>false,
                    'type'      =>'boolean',
                    'value'     =>'boolean'
                )
             ),
        ),
        'Doctrine\Common\Annotations\Annotation\Attributes' => array(
            'is_annotation'    => true,
            'has_constructor'  => false,
            'targets_literal'  => 'ANNOTATION_CLASS',
            'targets'          => Target::TARGET_CLASS,
            'default_property' => 'value',
            'properties'       => array(
                'value' => 'value'
            ),
            'attribute_types'  => array(
                'value' => array(
                    'type'      =>'array',
                    'required'  =>true,
                    'array_type'=>'Doctrine\Common\Annotations\Annotation\Attribute',
                    'value'     =>'array<Doctrine\Common\Annotations\Annotation\Attribute>'
                )
             ),
        ),
        'Doctrine\Common\Annotations\Annotation\Enum' => array(
            'is_annotation'    => true,
            'has_constructor'  => true,
            'targets_literal'  => 'ANNOTATION_PROPERTY',
            'targets'          => Target::TARGET_PROPERTY,
            'default_property' => 'value',
            'properties'       => array(
                'value' => 'value'
            ),
            'attribute_types'  => array(
                'value' => array(
                    'type'      => 'array',
                    'required'  => true,
                ),
                'literal' => array(
                    'type'      => 'array',
                    'required'  => false,
                ),
             ),
        ),
    );

    /**
     * Hash-map for handle types declaration
     *
     * @var array
     */
    private static $typeMap = array(
        'float'     => 'double',
        'bool'      => 'boolean',
        // allow uppercase Boolean in honor of George Boole
        'Boolean'   => 'boolean',
        'int'       => 'integer',
    );

    /**
     * Constructs a new DocParser.
     */
    public function __construct()
    {
        $this->lexer = new DocLexer;
    }

    /**
     * Sets the annotation names that are ignored during the parsing process.
     *
     * The names are supposed to be the raw names as used in the class, not the
     * fully qualified class names.
     *
     * @param array $names
     */
    public function setIgnoredAnnotationNames(array $names)
    {
        $this->ignoredAnnotationNames = $names;
    }

    /**
     * Sets ignore on not-imported annotations
     *
     * @param $bool
     */
    public function setIgnoreNotImportedAnnotations($bool)
    {
        $this->ignoreNotImportedAnnotations = (Boolean) $bool;
    }

    /**
     * Sets the default namespaces.
     *
     * @param array $namespace
     *
     * @throws \RuntimeException
     */
    public function addNamespace($namespace)
    {
        if ($this->imports) {
            throw new \RuntimeException('You must either use addNamespace(), or setImports(), but not both.');
        }
        $this->namespaces[] = $namespace;
    }

    /**
     * Sets the imports
     *
     * @param array $imports
     * @throws \RuntimeException
     */
    public function setImports(array $imports)
    {
        if ($this->namespaces) {
            throw new \RuntimeException('You must either use addNamespace(), or setImports(), but not both.');
        }
        $this->imports = $imports;
    }

     /**
     * Sets current target context as bitmask.
     *
     * @param integer $target
     */
    public function setTarget($target)
    {
        $this->target = $target;
    }

    /**
     * Parses the given docblock string for annotations.
     *
     * @param string $input The docblock string to parse.
     * @param string $context The parsing context.
     * @return array Array of annotations. If no annotations are found, an empty array is returned.
     */
    public function parse($input, $context = '')
    {
        if (false === $pos = strpos($input, '@')) {
            return array();
        }

        // also parse whatever character is before the @
        if ($pos > 0) {
            $pos -= 1;
        }

        $this->context = $context;
        $this->lexer->setInput(trim(substr($input, $pos), '* /'));
        $this->lexer->moveNext();

        return $this->Annotations();
    }

    /**
     * Attempts to match the given token with the current lookahead token.
     * If they match, updates the lookahead token; otherwise raises a syntax error.
     *
     * @param int $token type of Token.
     * @return bool True if tokens match; false otherwise.
     */
    private function match($token)
    {
        if ( ! $this->lexer->isNextToken($token) ) {
            $this->syntaxError($this->lexer->getLiteral($token));
        }

        return $this->lexer->moveNext();
    }

    /**
     * Attempts to match the current lookahead token with any of the given tokens.
     *
     * If any of them matches, this method updates the lookahead token; otherwise
     * a syntax error is raised.
     *
     * @param array $tokens
     * @return bool
     */
    private function matchAny(array $tokens)
    {
        if ( ! $this->lexer->isNextTokenAny($tokens)) {
            $this->syntaxError(implode(' or ', array_map(array($this->lexer, 'getLiteral'), $tokens)));
        }

        return $this->lexer->moveNext();
    }

    /**
     * Generates a new syntax error.
     *
     * @param string $expected Expected string.
     * @param array $token Optional token.
     *
     * @throws AnnotationException
     */
    private function syntaxError($expected, $token = null)
    {
        if ($token === null) {
            $token = $this->lexer->lookahead;
        }

        $message =  "Expected {$expected}, got ";

        if ($this->lexer->lookahead === null) {
            $message .= 'end of string';
        } else {
            $message .= "'{$token['value']}' at position {$token['position']}";
        }

        if (strlen($this->context)) {
            $message .= ' in ' . $this->context;
        }

        $message .= '.';

        throw AnnotationException::syntaxError($message);
    }

    /**
     * Attempt to check if a class exists or not. This never goes through the PHP autoloading mechanism
     * but uses the {@link AnnotationRegistry} to load classes.
     *
     * @param string $fqcn
     * @return boolean
     */
    private function classExists($fqcn)
    {
        if (isset($this->classExists[$fqcn])) {
            return $this->classExists[$fqcn];
        }

        // first check if the class already exists, maybe loaded through another AnnotationReader
        if (class_exists($fqcn, false)) {
            return $this->classExists[$fqcn] = true;
        }

        // final check, does this class exist?
        return $this->classExists[$fqcn] = AnnotationRegistry::loadAnnotationClass($fqcn);
    }

    /**
     * Collects parsing metadata for a given annotation class
     *
     * @param string $name The annotation name
     */
    private function collectAnnotationMetadata($name)
    {
        if (self::$metadataParser == null){
            self::$metadataParser = new self();
            self::$metadataParser->setIgnoreNotImportedAnnotations(true);
            self::$metadataParser->setIgnoredAnnotationNames($this->ignoredAnnotationNames);
            self::$metadataParser->setImports(array(
                'enum'          => 'Doctrine\Common\Annotations\Annotation\Enum',
                'target'        => 'Doctrine\Common\Annotations\Annotation\Target',
                'attribute'     => 'Doctrine\Common\Annotations\Annotation\Attribute',
                'attributes'    => 'Doctrine\Common\Annotations\Annotation\Attributes'
            ));
            AnnotationRegistry::registerFile(__DIR__ . '/Annotation/Enum.php');
            AnnotationRegistry::registerFile(__DIR__ . '/Annotation/Target.php');
            AnnotationRegistry::registerFile(__DIR__ . '/Annotation/Attribute.php');
            AnnotationRegistry::registerFile(__DIR__ . '/Annotation/Attributes.php');
        }

        $class      = new \ReflectionClass($name);
        $docComment = $class->getDocComment();

        // Sets default values for annotation metadata
        $metadata = array(
            'default_property' => null,
            'has_constructor'  => (null !== $constructor = $class->getConstructor()) && $constructor->getNumberOfParameters() > 0,
            'properties'       => array(),
            'property_types'   => array(),
            'attribute_types'  => array(),
            'targets_literal'  => null,
            'targets'          => Target::TARGET_ALL,
            'is_annotation'    => false !== strpos($docComment, '@Annotation'),
        );

        // verify that the class is really meant to be an annotation
        if ($metadata['is_annotation']) {

            self::$metadataParser->setTarget(Target::TARGET_CLASS);

            foreach (self::$metadataParser->parse($docComment, 'class @' . $name) as $annotation) {
                if ($annotation instanceof Target) {
                    $metadata['targets']         = $annotation->targets;
                    $metadata['targets_literal'] = $annotation->literal;

                } elseif ($annotation instanceof Attributes) {
                    foreach ($annotation->value as $attrib) {
                        // handle internal type declaration
                        $type = isset(self::$typeMap[$attrib->type]) ? self::$typeMap[$attrib->type] : $attrib->type;

                        // handle the case if the property type is mixed
                        if ('mixed' !== $type) {
                            // Checks if the property has array<type>
                            if (false !== $pos = strpos($type, '<')) {
                                $arrayType  = substr($type, $pos+1, -1);
                                $type       = 'array';

                                if (isset(self::$typeMap[$arrayType])) {
                                    $arrayType = self::$typeMap[$arrayType];
                                }

                                $metadata['attribute_types'][$attrib->name]['array_type'] = $arrayType;
                            }

                            $metadata['attribute_types'][$attrib->name]['type']     = $type;
                            $metadata['attribute_types'][$attrib->name]['value']    = $attrib->type;
                            $metadata['attribute_types'][$attrib->name]['required'] = $attrib->required;
                        }
                    }
                }
            }

            // if not has a constructor will inject values into public properties
            if (false === $metadata['has_constructor']) {
                // collect all public properties
                foreach ($class->getProperties(\ReflectionProperty::IS_PUBLIC) as $property) {
                    $metadata['properties'][$property->name] = $property->name;
                    
                    if(false === ($propertyComment = $property->getDocComment())) {
                        continue;
                    }

                    // checks if the property has @var annotation
                    if (false !== strpos($propertyComment, '@var')
                        && preg_match('/@var\s+([^\s]+)/',$propertyComment, $matches)) {
                        // literal type declaration
                        $value = $matches[1];

                        // handle internal type declaration
                        $type = isset(self::$typeMap[$value]) ? self::$typeMap[$value] : $value;

                        // handle the case if the property type is mixed
                        if ('mixed' !== $type) {
                            // Checks if the property has @var array<type> annotation
                            if (false !== $pos = strpos($type, '<')) {
                                $arrayType = substr($type, $pos+1, -1);
                                $type = 'array';

                                if (isset(self::$typeMap[$arrayType])) {
                                    $arrayType = self::$typeMap[$arrayType];
                                }

                                $metadata['attribute_types'][$property->name]['array_type'] = $arrayType;
                            }

                            $metadata['attribute_types'][$property->name]['type']       = $type;
                            $metadata['attribute_types'][$property->name]['value']      = $value;
                            $metadata['attribute_types'][$property->name]['required']   = false !== strpos($propertyComment, '@Required');
                        }
                    }

                    // checks if the property has @Enum
                    if (false !== strpos($propertyComment, '@Enum')){

                        $context = 'property ' . $class->name . "::\$" . $property->name;
                        self::$metadataParser->setTarget(Target::TARGET_PROPERTY);

                        foreach (self::$metadataParser->parse($propertyComment, $context) as $annotation) {
                            if($annotation instanceof Enum) {
                                $metadata['enum'][$property->name]['value']   = $annotation->value;
                                $metadata['enum'][$property->name]['literal'] = ! empty($annotation->literal) ? $annotation->literal : $annotation->value;
                            }
                        }
                    }
                }

                // choose the first property as default property
                $metadata['default_property'] = reset($metadata['properties']);
            }
        }

        self::$annotationMetadata[$name] = $metadata;
    }

    /**
     * Annotations ::= Annotation {[ "*" ]* [Annotation]}*
     *
     * @return array
     */
    private function Annotations()
    {
        $annotations = array();

        while (null !== $this->lexer->lookahead) {
            if (DocLexer::T_AT !== $this->lexer->lookahead['type']) {
                $this->lexer->moveNext();
                continue;
            }

            // make sure the @ is preceded by non-catchable pattern
            if (null !== $this->lexer->token && $this->lexer->lookahead['position'] === $this->lexer->token['position'] + strlen($this->lexer->token['value'])) {
                $this->lexer->moveNext();
                continue;
            }

            // make sure the @ is followed by either a namespace separator, or
            // an identifier token
            if ((null === $peek = $this->lexer->glimpse())
                || (DocLexer::T_NAMESPACE_SEPARATOR !== $peek['type'] && !in_array($peek['type'], self::$classIdentifiers, true))
                || $peek['position'] !== $this->lexer->lookahead['position'] + 1) {
                $this->lexer->moveNext();
                continue;
            }

            $this->isNestedAnnotation = false;
            if (false !== $annot = $this->Annotation()) {
                $annotations[] = $annot;
            }
        }

        return $annotations;
    }

    /**
     * Annotation     ::= "@" AnnotationName ["(" [Values] ")"]
     * AnnotationName ::= QualifiedName | SimpleName
     * QualifiedName  ::= NameSpacePart "\" {NameSpacePart "\"}* SimpleName
     * NameSpacePart  ::= identifier | null | false | true
     * SimpleName     ::= identifier | null | false | true
     *
     * @throws AnnotationException
     * @return mixed False if it is not a valid annotation.
     */
    private function Annotation()
    {
        $this->match(DocLexer::T_AT);

        // check if we have an annotation
        $name = $this->Identifier();

        // only process names which are not fully qualified, yet
        // fully qualified names must start with a \
        $originalName = $name;
        if ('\\' !== $name[0]) {
            $alias = (false === $pos = strpos($name, '\\'))? $name : substr($name, 0, $pos);

            $found = false;
            if ($this->namespaces) {
                foreach ($this->namespaces as $namespace) {
                    if ($this->classExists($namespace.'\\'.$name)) {
                        $name = $namespace.'\\'.$name;
                        $found = true;
                        break;
                    }
                }
            } elseif (isset($this->imports[$loweredAlias = strtolower($alias)])) {
                if (false !== $pos) {
                    $name = $this->imports[$loweredAlias].substr($name, $pos);
                } else {
                    $name = $this->imports[$loweredAlias];
                }
                $found = true;
            } elseif (isset($this->imports['__NAMESPACE__']) && $this->classExists($this->imports['__NAMESPACE__'].'\\'.$name)) {
                 $name = $this->imports['__NAMESPACE__'].'\\'.$name;
                 $found = true;
            } elseif ($this->classExists($name)) {
                $found = true;
            }

            if (!$found) {
                if ($this->ignoreNotImportedAnnotations || isset($this->ignoredAnnotationNames[$name])) {
                    return false;
                }

                throw AnnotationException::semanticalError(sprintf('The annotation "@%s" in %s was never imported. Did you maybe forget to add a "use" statement for this annotation?', $name, $this->context));
            }
        }

        if (!$this->classExists($name)) {
            throw AnnotationException::semanticalError(sprintf('The annotation "@%s" in %s does not exist, or could not be auto-loaded.', $name, $this->context));
        }

        // at this point, $name contains the fully qualified class name of the
        // annotation, and it is also guaranteed that this class exists, and
        // that it is loaded


        // collects the metadata annotation only if there is not yet
        if (!isset(self::$annotationMetadata[$name])) {
            $this->collectAnnotationMetadata($name);
        }

        // verify that the class is really meant to be an annotation and not just any ordinary class
        if (self::$annotationMetadata[$name]['is_annotation'] === false) {
            if (isset($this->ignoredAnnotationNames[$originalName])) {
                return false;
            }

            throw AnnotationException::semanticalError(sprintf('The class "%s" is not annotated with @Annotation. Are you sure this class can be used as annotation? If so, then you need to add @Annotation to the _class_ doc comment of "%s". If it is indeed no annotation, then you need to add @IgnoreAnnotation("%s") to the _class_ doc comment of %s.', $name, $name, $originalName, $this->context));
        }

        //if target is nested annotation
        $target = $this->isNestedAnnotation ? Target::TARGET_ANNOTATION : $this->target;

        // Next will be nested
        $this->isNestedAnnotation = true;

        //if annotation does not support current target
        if (0 === (self::$annotationMetadata[$name]['targets'] & $target) && $target) {
            throw AnnotationException::semanticalError(
                sprintf('Annotation @%s is not allowed to be declared on %s. You may only use this annotation on these code elements: %s.',
                     $originalName, $this->context, self::$annotationMetadata[$name]['targets_literal'])
            );
        }

        $values = array();
        if ($this->lexer->isNextToken(DocLexer::T_OPEN_PARENTHESIS)) {
            $this->match(DocLexer::T_OPEN_PARENTHESIS);

            if ( ! $this->lexer->isNextToken(DocLexer::T_CLOSE_PARENTHESIS)) {
                $values = $this->Values();
            }

            $this->match(DocLexer::T_CLOSE_PARENTHESIS);
        }

        if (isset(self::$annotationMetadata[$name]['enum'])) {
            // checks all declared attributes
            foreach (self::$annotationMetadata[$name]['enum'] as $property => $enum) {
                // checks if the attribute is a valid enumerator
                if (isset($values[$property]) && ! in_array($values[$property], $enum['value'])) {
                    throw AnnotationException::enumeratorError($property, $name, $this->context, $enum['literal'], $values[$property]);
                }
            }
        }

        // checks all declared attributes
        foreach (self::$annotationMetadata[$name]['attribute_types'] as $property => $type) {
            if ($property === self::$annotationMetadata[$name]['default_property']
                && !isset($values[$property]) && isset($values['value'])) {
                $property = 'value';
            }

            // handle a not given attribute or null value
            if (!isset($values[$property])) {
                if ($type['required']) {
                    throw AnnotationException::requiredError($property, $originalName, $this->context, 'a(n) '.$type['value']);
                }

                continue;
            }

            if ($type['type'] === 'array') {
                // handle the case of a single value
                if ( ! is_array($values[$property])) {
                    $values[$property] = array($values[$property]);
                }

                // checks if the attribute has array type declaration, such as "array<string>"
                if (isset($type['array_type'])) {
                    foreach ($values[$property] as $item) {
                        if (gettype($item) !== $type['array_type'] && !$item instanceof $type['array_type']) {
                            throw AnnotationException::typeError($property, $originalName, $this->context, 'either a(n) '.$type['array_type'].', or an array of '.$type['array_type'].'s', $item);
                        }
                    }
                }
            } elseif (gettype($values[$property]) !== $type['type'] && !$values[$property] instanceof $type['type']) {
                throw AnnotationException::typeError($property, $originalName, $this->context, 'a(n) '.$type['value'], $values[$property]);
            }
        }

        // check if the annotation expects values via the constructor,
        // or directly injected into public properties
        if (self::$annotationMetadata[$name]['has_constructor'] === true) {
            return new $name($values);
        }

        $instance = new $name();
        foreach ($values as $property => $value) {
            if (!isset(self::$annotationMetadata[$name]['properties'][$property])) {
                if ('value' !== $property) {
                    throw AnnotationException::creationError(sprintf('The annotation @%s declared on %s does not have a property named "%s". Available properties: %s', $originalName, $this->context, $property, implode(', ', self::$annotationMetadata[$name]['properties'])));
                }

                // handle the case if the property has no annotations
                if (!$property = self::$annotationMetadata[$name]['default_property']) {
                    throw AnnotationException::creationError(sprintf('The annotation @%s declared on %s does not accept any values, but got %s.', $originalName, $this->context, json_encode($values)));
                }
            }

            $instance->{$property} = $value;
        }

        return $instance;
    }

    /**
     * Values ::= Array | Value {"," Value}*
     *
     * @return array
     */
    private function Values()
    {
        $values = array();

        // Handle the case of a single array as value, i.e. @Foo({....})
        if ($this->lexer->isNextToken(DocLexer::T_OPEN_CURLY_BRACES)) {
            $values['value'] = $this->Value();
            return $values;
        }

        $values[] = $this->Value();

        while ($this->lexer->isNextToken(DocLexer::T_COMMA)) {
            $this->match(DocLexer::T_COMMA);
            $token = $this->lexer->lookahead;
            $value = $this->Value();

            if ( ! is_object($value) && ! is_array($value)) {
                $this->syntaxError('Value', $token);
            }

            $values[] = $value;
        }

        foreach ($values as $k => $value) {
            if (is_object($value) && $value instanceof \stdClass) {
                $values[$value->name] = $value->value;
            } else if ( ! isset($values['value'])){
                $values['value'] = $value;
            } else {
                if ( ! is_array($values['value'])) {
                    $values['value'] = array($values['value']);
                }

                $values['value'][] = $value;
            }

            unset($values[$k]);
        }

        return $values;
    }

    /**
     * Constant ::= integer | string | float | boolean
     *
     * @throws AnnotationException
     * @return mixed
     */
    private function Constant()
    {
        $identifier = $this->Identifier();

        if (!defined($identifier) && false !== strpos($identifier, '::') && '\\' !== $identifier[0]) {

            list($className, $const) = explode('::', $identifier);
            $alias = (false === $pos = strpos($className, '\\'))? $className : substr($className, 0, $pos);

            $found = false;
            switch (true) {
                case !empty ($this->namespaces):
                    foreach ($this->namespaces as $ns) {
                        if (class_exists($ns.'\\'.$className) || interface_exists($ns.'\\'.$className)) {
                             $className = $ns.'\\'.$className;
                             $found = true;
                             break;
                        }
                    }
                    break;

                case isset($this->imports[$loweredAlias = strtolower($alias)]):
                    $found = true;
                    if (false !== $pos) {
                        $className = $this->imports[$loweredAlias].substr($className, $pos);
                    } else {
                        $className = $this->imports[$loweredAlias];
                    }
                    break;

                default:
                    if(isset($this->imports['__NAMESPACE__'])) {
                        $ns = $this->imports['__NAMESPACE__'];
                        if (class_exists($ns.'\\'.$className) || interface_exists($ns.'\\'.$className)) {
                             $className = $ns.'\\'.$className;
                             $found = true;
                        }
                    }
                    break;
            }

            if ($found) {
                 $identifier = $className . '::' . $const;
            }
        }

        if (!defined($identifier)) {
            throw AnnotationException::semanticalErrorConstants($identifier, $this->context);
        }

        return constant($identifier);
    }

    /**
     * Identifier ::= string
     *
     * @return string
     */
    private function Identifier()
    {
        // check if we have an annotation
        if ($this->lexer->isNextTokenAny(self::$classIdentifiers)) {
            $this->lexer->moveNext();
            $className = $this->lexer->token['value'];
        } else {
            $this->syntaxError('namespace separator or identifier');
        }

        while ($this->lexer->lookahead['position'] === ($this->lexer->token['position'] + strlen($this->lexer->token['value']))
                && $this->lexer->isNextToken(DocLexer::T_NAMESPACE_SEPARATOR)) {

            $this->match(DocLexer::T_NAMESPACE_SEPARATOR);
            $this->matchAny(self::$classIdentifiers);
            $className .= '\\' . $this->lexer->token['value'];
        }

        return $className;
    }

    /**
     * Value ::= PlainValue | FieldAssignment
     *
     * @return mixed
     */
    private function Value()
    {
        $peek = $this->lexer->glimpse();

        if (DocLexer::T_EQUALS === $peek['type']) {
            return $this->FieldAssignment();
        }

        return $this->PlainValue();
    }

    /**
     * PlainValue ::= integer | string | float | boolean | Array | Annotation
     *
     * @return mixed
     */
    private function PlainValue()
    {
        if ($this->lexer->isNextToken(DocLexer::T_OPEN_CURLY_BRACES)) {
            return $this->Arrayx();
        }

        if ($this->lexer->isNextToken(DocLexer::T_AT)) {
            return $this->Annotation();
        }

        if ($this->lexer->isNextToken(DocLexer::T_IDENTIFIER)) {
            return $this->Constant();
        }

        switch ($this->lexer->lookahead['type']) {
            case DocLexer::T_STRING:
                $this->match(DocLexer::T_STRING);
                return $this->lexer->token['value'];

            case DocLexer::T_INTEGER:
                $this->match(DocLexer::T_INTEGER);
                return (int)$this->lexer->token['value'];

            case DocLexer::T_FLOAT:
                $this->match(DocLexer::T_FLOAT);
                return (float)$this->lexer->token['value'];

            case DocLexer::T_TRUE:
                $this->match(DocLexer::T_TRUE);
                return true;

            case DocLexer::T_FALSE:
                $this->match(DocLexer::T_FALSE);
                return false;

            case DocLexer::T_NULL:
                $this->match(DocLexer::T_NULL);
                return null;

            default:
                $this->syntaxError('PlainValue');
        }
    }

    /**
     * FieldAssignment ::= FieldName "=" PlainValue
     * FieldName ::= identifier
     *
     * @return array
     */
    private function FieldAssignment()
    {
        $this->match(DocLexer::T_IDENTIFIER);
        $fieldName = $this->lexer->token['value'];

        $this->match(DocLexer::T_EQUALS);

        $item = new \stdClass();
        $item->name  = $fieldName;
        $item->value = $this->PlainValue();

        return $item;
    }

    /**
     * Array ::= "{" ArrayEntry {"," ArrayEntry}* [","] "}"
     *
     * @return array
     */
    private function Arrayx()
    {
        $array = $values = array();

        $this->match(DocLexer::T_OPEN_CURLY_BRACES);

        // If the array is empty, stop parsing and return.
        if ($this->lexer->isNextToken(DocLexer::T_CLOSE_CURLY_BRACES)) {
            $this->match(DocLexer::T_CLOSE_CURLY_BRACES);

            return $array;
        }

        $values[] = $this->ArrayEntry();

        while ($this->lexer->isNextToken(DocLexer::T_COMMA)) {
            $this->match(DocLexer::T_COMMA);

            // optional trailing comma
            if ($this->lexer->isNextToken(DocLexer::T_CLOSE_CURLY_BRACES)) {
                break;
            }

            $values[] = $this->ArrayEntry();
        }

        $this->match(DocLexer::T_CLOSE_CURLY_BRACES);

        foreach ($values as $value) {
            list ($key, $val) = $value;

            if ($key !== null) {
                $array[$key] = $val;
            } else {
                $array[] = $val;
            }
        }

        return $array;
    }

    /**
     * ArrayEntry ::= Value | KeyValuePair
     * KeyValuePair ::= Key ("=" | ":") PlainValue | Constant
     * Key ::= string | integer | Constant
     *
     * @return array
     */
    private function ArrayEntry()
    {
        $peek = $this->lexer->glimpse();

        if (DocLexer::T_EQUALS === $peek['type']
                || DocLexer::T_COLON === $peek['type']) {

            if ($this->lexer->isNextToken(DocLexer::T_IDENTIFIER)) {
                $key = $this->Constant();
            } else {
                $this->matchAny(array(DocLexer::T_INTEGER, DocLexer::T_STRING));
                $key = $this->lexer->token['value'];
            }

            $this->matchAny(array(DocLexer::T_EQUALS, DocLexer::T_COLON));

            return array($key, $this->PlainValue());
        }

        return array(null, $this->Value());
    }
}
