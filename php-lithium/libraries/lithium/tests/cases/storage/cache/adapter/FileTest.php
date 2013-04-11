<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\tests\cases\storage\cache\adapter;

use SplFileInfo;
use lithium\core\Libraries;
use lithium\storage\cache\adapter\File;

class FileTest extends \lithium\test\Unit {

	/**
	 * Checks whether the 'empty' file exists in `resources/tmp/cache` and, if so, ensures
	 * that it is restored at the end of the testing cycle.
	 *
	 * @var string
	 */
	protected $_hasEmpty = true;

	/**
	 * Skip the test if the default File adapter read/write path
	 * is not read/write-able.
	 *
	 * @return void
	 */
	public function skip() {
		$directory = new SplFileInfo(Libraries::get(true, 'resources') . "/tmp/cache/");
		$accessible = ($directory->isDir() && $directory->isReadable() && $directory->isWritable());
		$message = 'The File cache adapter path does not have the proper permissions.';
		$this->skipIf(!$accessible, $message);
	}

	public function setUp() {
		$this->_hasEmpty = file_exists(Libraries::get(true, 'resources') . "/tmp/cache/empty");
		$this->File = new File();
	}

	public function tearDown() {
		$resources = realpath(Libraries::get(true, 'resources'));
		$paths = array("{$resources}/tmp/cache", "{$resources}/tmp/cache/templates");

		if ($this->_hasEmpty) {
			foreach ($paths as $path) {
				$path = realpath($path);
				if (is_dir($path) && is_writable($path)) {
					touch("{$resources}/empty");
				}
			}
		}
		unset($this->File);
	}

	public function testEnabled() {
		$file = $this->File;
		$this->assertTrue($file::enabled());
	}

	public function testWrite() {
		$key = 'key';
		$data = 'data';
		$expiry = '+1 minute';
		$time = time() + 60;

		$closure = $this->File->write($key, $data, $expiry);
		$this->assertTrue(is_callable($closure));

		$params = compact('key', 'data', 'expiry');
		$result = $closure($this->File, $params, null);
		$expected = 25;
		$this->assertEqual($expected, $result);

		$this->assertTrue(file_exists(Libraries::get(true, 'resources') . "/tmp/cache/{$key}"));
		$this->assertEqual(
			file_get_contents(Libraries::get(true, 'resources') . "/tmp/cache/{$key}"),
			"{:expiry:$time}\ndata"
		);

		$this->assertTrue(unlink(Libraries::get(true, 'resources') . "/tmp/cache/{$key}"));
		$this->assertFalse(file_exists(Libraries::get(true, 'resources') . "/tmp/cache/{$key}"));
	}

	public function testWriteDefaultCacheExpiry() {
		$file = new File(array('expiry' => '+1 minute'));
		$key = 'default_keykey';
		$data = 'data';
		$time = time() + 60;

		$closure = $file->write($key, $data);
		$this->assertTrue(is_callable($closure));

		$params = compact('key', 'data');
		$result = $closure($file, $params, null);
		$expected = 25;
		$this->assertEqual($expected, $result);

		$this->assertTrue(file_exists(Libraries::get(true, 'resources') . "/tmp/cache/{$key}"));
		$this->assertEqual(
			file_get_contents(Libraries::get(true, 'resources') . "/tmp/cache/{$key}"),
			"{:expiry:{$time}}\ndata"
		);

		$this->assertTrue(unlink(Libraries::get(true, 'resources') . "/tmp/cache/{$key}"));
		$this->assertFalse(file_exists(Libraries::get(true, 'resources') . "/tmp/cache/{$key}"));
	}

	public function testRead() {
		$key = 'key';
		$time = time() + 60;

		$closure = $this->File->read($key);
		$this->assertTrue(is_callable($closure));

		$path = Libraries::get(true, 'resources') . "/tmp/cache/{$key}";
		file_put_contents($path, "{:expiry:$time}\ndata");
		$this->assertTrue(file_exists($path));

		$params = compact('key');
		$result = $closure($this->File, $params, null);
		$this->assertEqual('data', $result);

		unlink($path);

		$key = 'non_existent';
		$params = compact('key');
		$closure = $this->File->read($key);
		$this->assertTrue(is_callable($closure));

		$result = $closure($this->File, $params, null);
		$this->assertFalse($result);
	}

	public function testExpiredRead() {
		$key = 'expired_key';
		$time = time() + 1;

		$closure = $this->File->read($key);
		$this->assertTrue(is_callable($closure));
		$path = Libraries::get(true, 'resources') . "/tmp/cache/{$key}";

		file_put_contents($path, "{:expiry:$time}\ndata");
		$this->assertTrue(file_exists($path));

		sleep(2);
		$params = compact('key');
		$this->assertFalse($closure($this->File, $params, null));

	}

	public function testDelete() {
		$key = 'key_to_delete';
		$time = time() + 1;
		$path = Libraries::get(true, 'resources') . "/tmp/cache/{$key}";

		file_put_contents($path, "{:expiry:$time}\ndata");
		$this->assertTrue(file_exists($path));

		$closure = $this->File->delete($key);
		$this->assertTrue(is_callable($closure));

		$params = compact('key');
		$this->assertTrue($closure($this->File, $params, null));

		$key = 'non_existent';
		$params = compact('key');
		$this->assertFalse($closure($this->File, $params, null));
	}

	public function testClear() {
		$key = 'key_to_clear';
		$time = time() + 1;
		$path = Libraries::get(true, 'resources') . "/tmp/cache/{$key}";
		file_put_contents($path, "{:expiry:$time}\ndata");

		$result = $this->File->clear();
		$this->assertTrue($result);
		$this->assertFalse(file_exists($path));

		$result = touch(Libraries::get(true, 'resources') . "/tmp/cache/empty");
		$this->assertTrue($result);
	}

	public function testIncrement() {
		$key = 'key_to_increment';
		$result = $this->File->increment($key);
		$this->assertEqual(false, $result);
	}

	public function testDecrement() {
		$key = 'key_to_decrement';
		$result = $this->File->decrement($key);
		$this->assertEqual(false, $result);
	}
}

?>