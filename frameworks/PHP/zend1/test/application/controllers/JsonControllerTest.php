<?php

class JsonControllerTest extends Zend_Test_PHPUnit_ControllerTestCase
{
    protected function setUp()
    {
        $this->bootstrap = new Zend_Application(
            APPLICATION_ENV,
            APPLICATION_PATH . '/configs/application.ini'
        );

        parent::setUp();
    }

    public function testType1JsonSerialization()
    {
        $this->dispatch('/json');

        $this->assertModule('default');
        $this->assertController('json');
        $this->assertAction('index');
        $this->assertResponseCode(200);
        $this->assertHeaderRegex('Content-Type', '#^application/json$#');

        $content = $this->getResponse()->getBody();

        $this->assertSame('{"message":"Hello, World!"}', $content);
        $this->assertEquals(27, iconv_strlen($content, 'UTF-8'));
    }
}
