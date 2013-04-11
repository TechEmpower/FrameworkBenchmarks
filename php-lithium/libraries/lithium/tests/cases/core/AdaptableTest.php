<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\core;

use SplDoublyLinkedList;
use lithium\core\Adaptable;
use lithium\storage\cache\adapter\Memory;
use lithium\tests\mocks\core\MockAdapter;
use lithium\tests\mocks\core\MockStrategy;
use lithium\tests\mocks\storage\cache\strategy\MockSerializer;
use lithium\tests\mocks\storage\cache\strategy\MockConfigurizer;

class AdaptableTest extends \lithium\test\Unit {

	public function setUp() {
		$this->adaptable = new Adaptable();
	}

	public function testConfig() {
		$this->assertFalse($this->adaptable->config());

		$items = array(array(
			'adapter' => 'some\adapter',
			'filters' => array('filter1', 'filter2')
		));
		$result = $this->adaptable->config($items);
		$this->assertNull($result);

		$expected = $items;
		$result = $this->adaptable->config();
		$this->assertEqual($expected, $result);

		$items = array(array(
			'adapter' => 'some\adapter',
			'filters' => array('filter1', 'filter2')
		));
		$this->adaptable->config($items);
		$result = $this->adaptable->config();
		$expected = $items;
		$this->assertEqual($expected, $result);
	}

	public function testReset() {
		$items = array(array(
			'adapter' => '\some\adapter',
			'filters' => array('filter1', 'filter2')
		));
		$this->adaptable->config($items);
		$result = $this->adaptable->config();
		$expected = $items;
		$this->assertEqual($expected, $result);

		$result = $this->adaptable->reset();
		$this->assertNull($result);
		$this->assertFalse($this->adaptable->config());
	}

	public function testNonExistentConfig() {
		$adapter = new MockAdapter();
		$this->expectException("Configuration `non_existent_config` has not been defined.");
		$adapter::adapter('non_existent_config');
	}

	public function testAdapter() {
		$adapter = new MockAdapter();
		$items = array('default' => array('adapter' => 'Memory', 'filters' => array()));
		$adapter::config($items);
		$result = $adapter::config();
		$expected = $items;
		$this->assertEqual($expected, $result);

		$result = $adapter::adapter('default');
		$expected = new Memory($items['default']);
		$this->assertEqual($expected, $result);
	}

	public function testConfigAndAdapter() {
		$adapter = new MockAdapter();
		$items = array('default' => array('adapter' => 'Memory', 'filters' => array()));
		$adapter::config($items);
		$config = $adapter::config();

		$intermediate = $adapter::adapter('default');
		$expected = new Memory($items['default']);
		$this->assertEqual($expected, $intermediate);

		$result = $adapter::config();
		$modified['default'] = $config['default'] + array('object' => $intermediate);
		$this->assertEqual($modified, $result);

		$adapter::config(array('default' => array('adapter' => 'Memory')));
		$result = $adapter::config();
		$this->assertEqual($config, $result);
	}

	public function testStrategy() {
		$strategy = new MockStrategy();
		$items = array('default' => array(
			'strategies' => array('lithium\tests\mocks\storage\cache\strategy\MockSerializer'),
			'filters' => array(),
			'adapter' => null
		));
		$strategy::config($items);
		$result = $strategy::config();
		$expected = $items;
		$this->assertEqual($expected, $result);

		$result = $strategy::strategies('default');
		$this->assertTrue($result instanceof SplDoublyLinkedList);
		$this->assertEqual(count($result), 1);
		$this->assertTrue($result->top() instanceof MockSerializer);
	}

	public function testInvalidStrategy() {
		$strategy = new MockStrategy();
		$items = array('default' => array(
			'strategies' => array('InvalidStrategy'),
			'filters' => array(),
			'adapter' => null
		));
		$strategy::config($items);

		$class = 'lithium\tests\mocks\core\MockStrategy';
		$message = "Could not find strategy `InvalidStrategy` in class `{$class}`.";
		$this->expectException($message);

		$result = $strategy::strategies('default');
		$this->assertTrue($result instanceof SplDoublyLinkedList);
	}

	public function testStrategyConstructionSettings() {
		$strategy = new MockStrategy();
		$items = array('default' => array(
			'strategies' => array(
				'lithium\tests\mocks\storage\cache\strategy\MockConfigurizer' => array(
					'key1' => 'value1', 'key2' => 'value2'
				)
			),
			'filters' => array(),
			'adapter' => null
		));
		$strategy::config($items);
		$result = $strategy::config();
		$expected = $items;
		$this->assertEqual($expected, $result);

		$result = $strategy::strategies('default');
		$this->assertTrue($result instanceof SplDoublyLinkedList);
		$this->assertEqual(count($result), 1);
		$this->assertTrue($result->top() instanceof MockConfigurizer);
	}

	public function testNonExistentStrategyConfiguration() {
		$strategy = new MockStrategy();
		$this->expectException("Configuration `non_existent_config` has not been defined.");
		$result = $strategy::strategies('non_existent_config');
		$this->assertNull($result);
	}

	public function testApplyStrategiesNonExistentConfiguration() {
		$strategy = new MockStrategy();
		$this->expectException("Configuration `non_existent_config` has not been defined.");
		$strategy::applyStrategies('method', 'non_existent_config', null);
	}

	public function testApplySingleStrategy() {
		$strategy = new MockStrategy();
		$items = array('default' => array(
			'filters' => array(),
			'adapter' => null,
			'strategies' => array('lithium\tests\mocks\storage\cache\strategy\MockSerializer')
		));
		$strategy::config($items);
		$result = $strategy::config();
		$expected = $items;
		$this->assertEqual($expected, $result);

		$data = array('some' => 'data');
		$result = $strategy::applyStrategies('write', 'default', $data);
		$this->assertEqual(serialize($data), $result);
	}

	public function testApplySingleStrategyWithConfiguration() {
		$strategy = new MockStrategy();
		$params = array('key1' => 'value1', 'key2' => 'value2');
		$items = array('default' => array(
			'filters' => array(),
			'adapter' => null,
			'strategies' => array(
				'lithium\tests\mocks\storage\cache\strategy\MockConfigurizer' => $params
			)
		));
		$strategy::config($items);
		$result = $strategy::config();
		$expected = $items;
		$this->assertEqual($expected, $result);

		$result = $strategy::applyStrategies('write', 'default', null);
		$this->assertEqual($params, $result);
	}

	public function testApplyMultipleStrategies() {
		$strategy = new MockStrategy();
		$items = array('default' => array(
			'filters' => array(),
			'adapter' => null,
			'strategies' => array(
				'lithium\tests\mocks\storage\cache\strategy\MockSerializer', 'Base64'
			)
		));
		$strategy::config($items);
		$result = $strategy::config();
		$expected = $items;
		$this->assertEqual($expected, $result);

		$data = array('some' => 'data');
		$result = $strategy::applyStrategies('write', 'default', $data);
		$transformed = base64_encode(serialize($data));
		$this->assertEqual($transformed, $result);

		$options = array('mode' => 'LIFO');
		$result = $strategy::applyStrategies('read', 'default', $transformed, $options);
		$expected = $data;
		$this->assertEqual($expected, $result);
	}

	public function testApplyStrategiesNoConfiguredStrategies() {
		$strategy = new MockStrategy();

		$items = array('default' => array(
			'filters' => array(),
			'adapter' => null
		));
		$strategy::config($items);
		$result = $strategy::config();
		$expected = $items;
		$this->assertEqual($expected, $result);

		$result = $strategy::applyStrategies('method', 'default', null);
		$this->assertNull($result);

		$items = array('default' => array(
			'filters' => array(),
			'adapter' => null,
			'strategies' => array()
		));
		$strategy::config($items);
		$result = $strategy::config();
		$expected = $items;
		$this->assertEqual($expected, $result);

		$data = array('some' => 'data');
		$result = $strategy::applyStrategies('method', 'default', $data);
		$this->assertEqual($data, $result);
	}

	public function testEnabled() {
		$adapter = new MockAdapter();

		$items = array('default' => array('adapter' => 'Memory', 'filters' => array()));
		$adapter::config($items);
		$result = $adapter::config();
		$expected = $items;
		$this->assertEqual($expected, $result);

		$result = $adapter::adapter('default');
		$expected = new Memory($items['default']);
		$this->assertEqual($expected, $result);

		$this->assertTrue($adapter::enabled('default'));
		$this->assertNull($adapter::enabled('non-existent'));
	}

	public function testNonExistentAdapter() {
		$adapter = new MockAdapter();

		$items = array('default' => array('adapter' => 'NonExistent', 'filters' => array()));
		$adapter::config($items);
		$result = $adapter::config();
		$expected = $items;
		$this->assertEqual($expected, $result);

		$message  = 'Could not find adapter `NonExistent` in ';
		$message .= 'class `lithium\tests\mocks\core\MockAdapter`.';
		$this->expectException($message);

		$result = $adapter::adapter('default');
		$this->assertNull($result);
	}

	public function testEnvironmentSpecificConfiguration() {
		$adapter = new MockAdapter();
		$config = array('adapter' => 'Memory', 'filters' => array());
		$items = array('default' => array(
			'development' => $config, 'test' => $config, 'production' => $config
		));
		$adapter::config($items);
		$result = $adapter::config();
		$expected = array('default' => $config);
		$this->assertEqual($expected, $result);

		$result = $adapter::config('default');
		$expected = $config;
		$this->assertEqual($expected, $result);

		$result = $adapter::adapter('default');
		$expected = new Memory($config);
		$this->assertEqual($expected, $result);
	}

	public function testConfigurationNoAdapter() {
		$adapter = new MockAdapter();
		$items = array('default' => array('filters' => array()));
		$adapter::config($items);

		$message  = 'No adapter set for configuration in ';
		$message .= 'class `lithium\tests\mocks\core\MockAdapter`.';
		$this->expectException($message);
		$result = $adapter::adapter('default');
	}
}

?>