<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\storage\cache\adapter;

use SplFileInfo;
use RecursiveIteratorIterator;
use RecursiveDirectoryIterator;
use lithium\core\Libraries;

/**
 * A minimal file-based cache.
 *
 * This File adapter provides basic support for `write`, `read`, `delete`
 * and `clear` cache functionality, as well as allowing the first four
 * methods to be filtered as per the Lithium filtering system. The File adapter
 * is a very simple cache, and should only be used for prototyping or for specifically
 * caching _files_. For more general caching needs, please consider using a more
 * appropriate cache adapter.
 *
 * This adapter does *not* provide increment/decrement functionality. For such
 * functionality, please use a more appropriate cache adapter.
 *
 * This adapter does *not* allow multi-key operations for any methods.
 *
 * The path that the cached files will be written to defaults to
 * `<app>/resources/tmp/cache`, but is user-configurable on cache configuration.
 *
 * Note that the cache expiration time is stored within the first few bytes
 * of the cached data, and is transparently added and/or removed when values
 * are stored and/or retrieved from the cache.
 *
 * @see lithium\storage\cache\adapter
 */
class File extends \lithium\core\Object {

	/**
	 * Class constructor.
	 *
	 * @see lithium\storage\Cache::config()
	 * @param array $config Configuration parameters for this cache adapter. These settings are
	 *        indexed by name and queryable through `Cache::config('name')`.
	 *        The defaults are:
	 *        - 'path' : Path where cached entries live `LITHIUM_APP_PATH . '/resources/tmp/cache'`.
	 *        - 'expiry' : Default expiry time used if none is explicitly set when calling
	 *          `Cache::write()`.
	 */
	public function __construct(array $config = array()) {
		$defaults = array(
			'path' => Libraries::get(true, 'resources') . '/tmp/cache',
			'prefix' => '',
			'expiry' => '+1 hour'
		);
		parent::__construct($config + $defaults);
	}

	/**
	 * Write value(s) to the cache.
	 *
	 * @param string $key The key to uniquely identify the cached item.
	 * @param mixed $data The value to be cached.
	 * @param null|string $expiry A strtotime() compatible cache time. If no expiry time is set,
	 *        then the default cache expiration time set with the cache configuration will be used.
	 * @return closure Function returning boolean `true` on successful write, `false` otherwise.
	 */
	public function write($key, $data, $expiry = null) {
		$path = $this->_config['path'];
		$expiry = ($expiry) ?: $this->_config['expiry'];

		return function($self, $params) use (&$path, $expiry) {
			$expiry = strtotime($expiry);
			$data = "{:expiry:{$expiry}}\n{$params['data']}";
			$path = "{$path}/{$params['key']}";
			return file_put_contents($path, $data);
		};
	}

	/**
	 * Read value(s) from the cache.
	 *
	 * @param string $key The key to uniquely identify the cached item.
	 * @return closure Function returning cached value if successful, `false` otherwise.
	 */
	public function read($key) {
		$path = $this->_config['path'];

		return function($self, $params) use (&$path) {
			extract($params);
			$path = "$path/$key";
			$file = new SplFileInfo($path);

			if (!$file->isFile() || !$file->isReadable()) {
				return false;
			}

			$data = file_get_contents($path);
			preg_match('/^\{\:expiry\:(\d+)\}\\n/', $data, $matches);
			$expiry = $matches[1];

			if ($expiry < time()) {
				unlink($path);
				return false;
			}
			return preg_replace('/^\{\:expiry\:\d+\}\\n/', '', $data, 1);
		};
	}

	/**
	 * Delete an entry from the cache.
	 *
	 * @param string $key The key to uniquely identify the cached item.
	 * @return closure Function returning boolean `true` on successful delete, `false` otherwise.
	 */
	public function delete($key) {
		$path = $this->_config['path'];

		return function($self, $params) use (&$path) {
			extract($params);
			$path = "$path/$key";
			$file = new SplFileInfo($path);

			if ($file->isFile() && $file->isReadable()) {
				return unlink($path);
			}
			return false;
		};
	}

	/**
	 * The File adapter does not provide any facilities for atomic incrementing
	 * of cache items. If you need this functionality, please use a cache adapter
	 * which provides native support for atomic increment.
	 *
	 * This method is not implemented, and will simply return false.
	 *
	 * @param string $key Key of numeric cache item to increment
	 * @param integer $offset Offset to increment - defaults to 1.
	 * @return boolean False - this method is not implemented
	 */
	public function increment($key, $offset = 1) {
		return false;
	}

	/**
	 * The File adapter does not provide any facilities for atomic decrementing
	 * of cache items. If you need this functionality, please use a cache adapter
	 * which provides native support for atomic decrement.
	 *
	 * This method is not implemented, and will simply return false.
	 *
	 * @param string $key Key of numeric cache item to decrement
	 * @param integer $offset Offset to increment - defaults to 1.
	 * @return boolean False - this method is not implemented
	 */
	public function decrement($key, $offset = 1) {
		return false;
	}

	/**
	 * Clears user-space cache.
	 *
	 * @return mixed True on successful clear, false otherwise.
	 */
	public function clear() {
		$base = new RecursiveDirectoryIterator($this->_config['path']);
		$iterator = new RecursiveIteratorIterator($base);

		foreach ($iterator as $file) {
			if ($file->isFile()) {
				unlink($file->getPathName());
			}
		}
		return true;
	}

	/**
	 * Implements cache adapter support-detection interface.
	 *
	 * @return boolean Always returns `true`.
	 */
	public static function enabled() {
		return true;
	}
}

?>