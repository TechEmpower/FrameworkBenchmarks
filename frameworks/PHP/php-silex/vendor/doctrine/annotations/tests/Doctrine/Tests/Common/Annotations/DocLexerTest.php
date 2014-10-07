<?php

namespace Doctrine\Tests\Common\Annotations;

use Doctrine\Common\Annotations\DocLexer;

class DocLexerTest extends \PHPUnit_Framework_TestCase
{
    public function testMarkerAnnotation()
    {
        $lexer = new DocLexer;

        $lexer->setInput("@Name");
        $this->assertNull($lexer->token);
        $this->assertNull($lexer->lookahead);

        $this->assertTrue($lexer->moveNext());
        $this->assertNull($lexer->token);
        $this->assertEquals('@', $lexer->lookahead['value']);

        $this->assertTrue($lexer->moveNext());
        $this->assertEquals('@', $lexer->token['value']);
        $this->assertEquals('Name', $lexer->lookahead['value']);

        $this->assertFalse($lexer->moveNext());
    }

    public function testScannerTokenizesDocBlockWhitConstants()
    {
        $lexer      = new DocLexer();
        $docblock   = '@AnnotationWithConstants(PHP_EOL, ClassWithConstants::SOME_VALUE, \Doctrine\Tests\Common\Annotations\Fixtures\IntefaceWithConstants::SOME_VALUE)';

        $tokens = array (
            array(
                'value'     => '@',
                'position'  => 0,
                'type'      => DocLexer::T_AT,
            ),
            array(
                'value'     => 'AnnotationWithConstants',
                'position'  => 1,
                'type'      => DocLexer::T_IDENTIFIER,
            ),
            array(
                'value'     => '(',
                'position'  => 24,
                'type'      => DocLexer::T_OPEN_PARENTHESIS,
            ),
            array(
                'value'     => 'PHP_EOL',
                'position'  => 25,
                'type'      => DocLexer::T_IDENTIFIER,
            ),
            array(
                'value'     => ',',
                'position'  => 32,
                'type'      => DocLexer::T_COMMA,
            ),
            array(
                'value'     => 'ClassWithConstants::SOME_VALUE',
                'position'  => 34,
                'type'      => DocLexer::T_IDENTIFIER,
            ),
            array(
                'value'     => ',',
                'position'  => 64,
                'type'      => DocLexer::T_COMMA,
            ),
            array(
                'value'     => '\\Doctrine\\Tests\\Common\\Annotations\\Fixtures\\IntefaceWithConstants::SOME_VALUE',
                'position'  => 66,
                'type'      => DocLexer::T_IDENTIFIER,
            ),
            array(
                'value'     => ')',
                'position'  => 143,
                'type'      => DocLexer::T_CLOSE_PARENTHESIS,
            )

        );

        $lexer->setInput($docblock);

        foreach ($tokens as $expected) {
            $lexer->moveNext();
            $lookahead = $lexer->lookahead;
            $this->assertEquals($expected['value'],     $lookahead['value']);
            $this->assertEquals($expected['type'],      $lookahead['type']);
            $this->assertEquals($expected['position'],  $lookahead['position']);
        }

        $this->assertFalse($lexer->moveNext());
    }


    public function testScannerTokenizesDocBlockWhitInvalidIdentifier()
    {
        $lexer      = new DocLexer();
        $docblock   = '@Foo\3.42';

        $tokens = array (
            array(
                'value'     => '@',
                'position'  => 0,
                'type'      => DocLexer::T_AT,
            ),
            array(
                'value'     => 'Foo',
                'position'  => 1,
                'type'      => DocLexer::T_IDENTIFIER,
            ),
            array(
                'value'     => '\\',
                'position'  => 4,
                'type'      => DocLexer::T_NAMESPACE_SEPARATOR,
            ),
            array(
                'value'     => 3.42,
                'position'  => 5,
                'type'      => DocLexer::T_FLOAT,
            )
        );

        $lexer->setInput($docblock);

        foreach ($tokens as $expected) {
            $lexer->moveNext();
            $lookahead = $lexer->lookahead;
            $this->assertEquals($expected['value'],     $lookahead['value']);
            $this->assertEquals($expected['type'],      $lookahead['type']);
            $this->assertEquals($expected['position'],  $lookahead['position']);
        }

        $this->assertFalse($lexer->moveNext());
    }

}