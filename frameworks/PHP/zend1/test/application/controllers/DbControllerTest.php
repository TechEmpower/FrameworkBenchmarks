<?php

class DbControllerTest extends Zend_Test_PHPUnit_ControllerTestCase
{
    protected function setUp()
    {
        $this->bootstrap = new Zend_Application(
            APPLICATION_ENV,
            APPLICATION_PATH . '/configs/application.ini'
        );

        parent::setUp();
    }

    public function testType2SingleDatabaseQuery()
    {
        $this->dispatch('/db');

        $this->assertModule('default');
        $this->assertController('db');
        $this->assertAction('index');
        $this->assertResponseCode(200);
        $this->assertHeaderRegex('Content-Type', '#^application/json$#');

        $content = $this->getResponse()->getBody();

        $this->assertRegExp('#^{"id"\:"\d+","randomNumber":"\d+"}$#', $content);

        $decodedContent = json_decode($content, true);

        $this->assertTrue(json_last_error() === JSON_ERROR_NONE, 'Json decode failure');

        $this->assertArrayHasKey('id', $decodedContent);
        $this->assertGreaterThan(0, $decodedContent['id']);
        $this->assertLessThan(10000, $decodedContent['id']);

        $this->assertArrayHasKey('randomNumber', $decodedContent);
        $this->assertGreaterThan(0, $decodedContent['randomNumber']);
        $this->assertLessThan(10000, $decodedContent['randomNumber']);

        $this->assertCount(2, $decodedContent);
    }
}
