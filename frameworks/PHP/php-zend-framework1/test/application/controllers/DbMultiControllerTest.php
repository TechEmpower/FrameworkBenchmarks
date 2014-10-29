<?php

class DbMultiControllerTest extends Zend_Test_PHPUnit_ControllerTestCase
{
    protected function setUp()
    {
        $this->bootstrap = new Zend_Application(
            APPLICATION_ENV,
            APPLICATION_PATH . '/configs/application.ini'
        );

        parent::setUp();
    }

    public function testType3MultipleDatabaseQueries()
    {
        $this->dispatch('/db-multi?queries=2');
        $this->assertResponseResultsEquals(2, $this->getResponse()->getBody());
    }

    /**
     * Helper assertion
     *
     * @param  int $expectedCount
     * @param  string $content
     */
    protected function assertResponseResultsEquals($expectedCount, $content)
    {
        $this->assertModule('default');
        $this->assertController('db-multi');
        $this->assertAction('index');
        $this->assertResponseCode(200);
        $this->assertHeaderRegex('Content-Type', '#^application/json$#');

        $results = json_decode($content, true);

        $this->assertTrue(json_last_error() === JSON_ERROR_NONE, 'Json decode failure');

        $this->assertCount($expectedCount, $results);

        $count = count($results);
        for ($i = 0; $i < $count; $i++) {

            $this->assertArrayHasKey('id', $results[$i]);
            $this->assertGreaterThan(0, $results[$i]['id']);
            $this->assertLessThan(10000, $results[$i]['id']);

            $this->assertArrayHasKey('randomNumber', $results[$i]);
            $this->assertGreaterThan(0, $results[$i]['randomNumber']);
            $this->assertLessThan(10000, $results[$i]['randomNumber']);
        }
    }
}
