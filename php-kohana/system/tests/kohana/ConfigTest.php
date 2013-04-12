<?php defined('SYSPATH') OR die('Kohana bootstrap needs to be included before tests run');

/**
 * Tests the Config lib that's shipped with kohana
 *
 * @group kohana
 * @group kohana.core
 * @group kohana.core.config
 *
 * @package    Kohana
 * @category   Tests
 * @author     Kohana Team
 * @author     Jeremy Bush <contractfrombelow@gmail.com>
 * @author     Matt Button <matthew@sigswitch.com>
 * @copyright  (c) 2008-2012 Kohana Team
 * @license    http://kohanaframework.org/license
 */
class Kohana_ConfigTest extends Unittest_TestCase
{

	/**
	 * When a config object is initially created there should be
	 * no readers attached
	 *
	 * @test
	 * @covers Config
	 */
	public function test_initially_there_are_no_sources()
	{
		$config = new Config;

		$this->assertAttributeSame(array(), '_sources', $config);
	}

	/**
	 * Test that calling attach() on a kohana config object
	 * adds the specified reader to the config object
	 *
	 * @test
	 * @covers Config::attach
	 */
	public function test_attach_adds_reader_and_returns_this()
	{
		$config = new Config;
		$reader = $this->getMock('Kohana_Config_Reader');

		$this->assertSame($config, $config->attach($reader));

		$this->assertAttributeContains($reader, '_sources', $config);
	}

	/**
	 * By default (or by passing TRUE as the second parameter) the config object
	 * should prepend the reader to the front of the readers queue
	 *
	 * @test
	 * @covers Config::attach
	 */
	public function test_attach_adds_reader_to_front_of_queue()
	{
		$config  = new Config;

		$reader1 = $this->getMock('Kohana_Config_Reader');
		$reader2 = $this->getMock('Kohana_Config_Reader');

		$config->attach($reader1);
		$config->attach($reader2);

		// Rather than do two assertContains we'll do an assertSame to assert
		// the order of the readers
		$this->assertAttributeSame(array($reader2, $reader1), '_sources', $config);

		// Now we test using the second parameter
		$config = new Config;

		$config->attach($reader1);
		$config->attach($reader2, TRUE);

		$this->assertAttributeSame(array($reader2, $reader1), '_sources', $config);
	}

	/**
	 * Test that attaching a new reader (and passing FALSE as second param) causes
	 * phpunit to append the reader rather than prepend
	 *
	 * @test
	 * @covers Config::attach
	 */
	public function test_attach_can_add_reader_to_end_of_queue()
	{
		$config  = new Config;
		$reader1 = $this->getMock('Kohana_Config_Reader');
		$reader2 = $this->getMock('Kohana_Config_Reader');

		$config->attach($reader1);
		$config->attach($reader2, FALSE);

		$this->assertAttributeSame(array($reader1, $reader2), '_sources', $config);
	}

	/**
	 * Calling detach() on a config object should remove it from the queue of readers
	 *
	 * @test
	 * @covers Config::detach
	 */
	public function test_detach_removes_reader_and_returns_this()
	{
		$config  = new Config;

		// Due to the way phpunit mock generator works if you try and mock a class
		// that has already been used then it just re-uses the first's name

		// To get around this we have to specify a totally random name for the second mock object
		$reader1 = $this->getMock('Kohana_Config_Reader');
		$reader2 = $this->getMock('Kohana_Config_Reader', array(), array(), 'MY_AWESOME_READER');

		$config->attach($reader1);
		$config->attach($reader2);

		$this->assertSame($config, $config->detach($reader1));

		$this->assertAttributeNotContains($reader1, '_sources', $config);
		$this->assertAttributeContains($reader2, '_sources', $config);

		$this->assertSame($config, $config->detach($reader2));

		$this->assertAttributeNotContains($reader2, '_sources', $config);
	}

	/**
	 * detach() should return $this even if the specified reader does not exist
	 *
	 * @test
	 * @covers Config::detach
	 */
	public function test_detach_returns_this_even_when_reader_dnx()
	{
		$config = new Config;
		$reader = $this->getMock('Kohana_Config_Reader');

		$this->assertSame($config, $config->detach($reader));
	}

	/**
	 * If we request a config variable with a dot path then
	 * Config::load() should load the group and return the requested variable
	 *
	 * @test
	 * @covers Config::load
	 */
	public function test_load_can_get_var_from_dot_path()
	{
		$config = new Config;

		$reader = $this->getMock('Kohana_Config_Reader', array('load'));

		$reader
			->expects($this->once())
			->method('load')
			->with('beer')
			->will($this->returnValue(array('stout' => 'Guinness')));

		$config->attach($reader);

		$this->assertSame('Guinness', $config->load('beer.stout'));
	}

	/**
	 * If we've already loaded a config group then the correct variable
	 * should be returned if we use the dot path notation to to request 
	 * a var
	 *
	 * @test
	 * @covers Config::load
	 */
	public function test_load_can_get_var_from_dot_path_for_loaded_group()
	{
		$config = new Config;

		$reader = $this->getMock('Kohana_Config_Reader', array('load'));

		$reader
			->expects($this->once())
			->method('load')
			->with('beer')
			->will($this->returnValue(array('stout' => 'Guinness')));

		$config->attach($reader);

		$config->load('beer');

		$this->assertSame('Guinness', $config->load('beer.stout'));
	}

	/**
	 * If load() is called and there are no readers present then it should throw
	 * a kohana exception
	 *
	 * @test
	 * @covers Config::load
	 * @expectedException Kohana_Exception
	 */
	public function test_load_throws_exception_if_there_are_no_sources()
	{
		// The following code should throw an exception and phpunit will catch / handle it
		// (see the @expectedException doccomment)
		$config = new Kohana_config;

		$config->load('random');
	}

	/**
	 * Provides test data for test_load_throws_exception_if_no_group_is_given()
	 *
	 * @return array
	 */
	public function provider_load_throws_exception_if_no_group_is_given()
	{
		return array(
			array(NULL),
			array(''),
			array(array()),
			array(array('foo' => 'bar')),
			array(new StdClass),
		);
	}

	/**
	 * If an invalid group name is specified then an exception should be thrown.
	 *
	 * Invalid means it's either a non-string value, or empty
	 *
	 * @test
	 * @dataProvider provider_load_throws_exception_if_no_group_is_given
	 * @covers Config::load
	 * @expectedException Kohana_Exception
	 */
	public function test_load_throws_exception_if_invalid_group($value)
	{
		$config = new Kohana_Config;

		$reader = $this->getMock('Kohana_Config_Reader');

		$config->attach($reader);

		$config->load($value);
	}

	/**
	 * Make sure that _write_config() passes the changed configuration to all 
	 * writers in the queue
	 *
	 * @test
	 * @covers Kohana_Config
	 */
	public function test_write_config_passes_changed_config_to_all_writers()
	{
		$config = new Kohana_Config;

		$reader1 = $this->getMock('Kohana_Config_Reader');
		$writer1 = $this->getMock('Kohana_Config_Writer', array('write'));
		$writer2 = $this->getMock('Kohana_Config_Writer', array('write'));

		$writer1
			->expects($this->once())
			->method('write')
			->with('some_group', 'key', 'value');

		$writer2
			->expects($this->once())
			->method('write')
			->with('some_group', 'key', 'value');

		$config->attach($reader1)->attach($writer1)->attach($writer2);

		$config->_write_config('some_group', 'key', 'value');
	}

	/**
	 * Config sources are stored in a stack, make sure that config at the bottom
	 * of the stack is overriden by config at the top
	 *
	 * @test
	 * @covers Config::load
	 */
	public function test_config_is_loaded_from_top_to_bottom_of_stack()
	{
		$group_name =  'lolumns';

		$reader1 = $this->getMock('Kohana_Config_Reader', array('load'), array(), 'Unittest_Config_Reader_1');
		$reader2 = $this->getMock('Kohana_Config_Reader', array('load'), array(), 'Unittest_Config_Reader_2');

		$reader1
			->expects($this->once())
			->method('load')
			->with($group_name)
			->will($this->returnValue(array('foo' => 'bar', 'kohana' => 'awesome', 'life' => array('normal', 'fated'))));

		$reader2
			->expects($this->once())
			->method('load')
			->with($group_name)
			->will($this->returnValue(array('kohana' => 'sweet', 'music' => 'tasteful', 'life' => array('extraordinary', 'destined'))));

		$config = new Kohana_Config;

		// Attach $reader1 at the "top" and reader2 at the "bottom"
		$config->attach($reader1)->attach($reader2, FALSE);

		$this->assertSame(
			array(
				'kohana' => 'awesome',
				'music'  => 'tasteful',
				'life'   => array(
					'extraordinary',
					'destined',
					'normal',
					'fated',
				),
				'foo'    => 'bar',
			),
			$config->load($group_name)->as_array()
		);
	}

	/**
	 * load() should keep a record of what config groups have been requested and if
	 * a group is requested more than once the first instance should be returned
	 *
	 * @test
	 * @covers Config::load
	 */
	public function test_load_reuses_config_groups()
	{
		$reader = $this->getMock('Kohana_Config_Reader', array('load'));
		$reader
			->expects($this->once())
			->method('load')
			->with('something')
			->will($this->returnValue(array()));

		$config = new Kohana_Config;

		$config->attach($reader);

		$group = $config->load('something');

		$this->assertSame($group, $config->load('something'));
	}

	/**
	 * When we call copy() we expect it to copy the merged config to all writers
	 *
	 * @TODO This test sucks due to limitations in the phpunit mock generator.  MAKE THIS AWESOME AGAIN!
	 * @test
	 * @covers Kohana_Config::copy
	 */
	public function test_copy_copies_merged_config_to_all_writers()
	{
		$config = new Kohana_Config;

		$reader1 = $this->getMock('Kohana_Config_Reader', array('load'));
		$reader2 = $this->getMock('Kohana_Config_Reader', array('load'));

		$reader1
			->expects($this->once())
			->method('load')
			->with('something')
			->will($this->returnValue(array('pie' => 'good', 'kohana' => 'awesome')));

		$reader2
			->expects($this->once())
			->method('load')
			->with('something')
			->will($this->returnValue(array('kohana' => 'good')));

		$writer1 = $this->getMock('Kohana_Config_Writer', array('write'));
		$writer2 = $this->getMock('Kohana_Config_Writer', array('write'));

		// Due to crazy limitations in phpunit's mocking engine we have to be fairly
		// liberal here as to what order we receive the config items
		// Good news is that order shouldn't matter *yay*
		// 
		// Now save your eyes and skip the next... 13 lines!
		$key = $this->logicalOr('pie', 'kohana');
		$val = $this->logicalOr('good', 'awesome');

		$writer1
			->expects($this->exactly(2))
			->method('write')
			->with('something', clone $key, clone $val);

		$writer2
			->expects($this->exactly(2))
			->method('write')
			->with('something', clone $key, clone $val);

		$config
			->attach($reader1)->attach($reader2, FALSE)
			->attach($writer1)->attach($writer2);

		// Now let's get this thing going!
		$config->copy('something');
	}
}
