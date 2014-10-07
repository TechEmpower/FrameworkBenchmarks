<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

/**
 * This file creates a default cache configuration using the most optimized adapter available, and
 * uses it to provide default caching for high-overhead operations.
 */
use lithium\storage\Cache;
use lithium\core\Libraries;
use lithium\core\Environment;
use lithium\action\Dispatcher;
use lithium\storage\cache\adapter\Apc;

/**
 * If APC is not available and the cache directory is not writeable, bail out. This block should be
 * removed post-install, and the cache should be configured with the adapter you plan to use.
 */
$cachePath = Libraries::get(true, 'resources') . '/tmp/cache';

if (!(($apcEnabled = Apc::enabled()) || PHP_SAPI === 'cli') && !is_writable($cachePath)) {
	return;
}

/**
 * This configures the default cache, based on whether ot not APC user caching is enabled. If it is
 * not, file caching will be used. Most of this code is for getting you up and running only, and
 * should be replaced with a hard-coded configuration, based on the cache(s) you plan to use.
 */
Cache::config(array('default' => $apcEnabled ? array('adapter' => 'Apc') : array(
	'adapter' => 'File', 'strategies' => array('Serializer')
)));

/**
 * Caches paths for auto-loaded and service-located classes when in production.
 */
Dispatcher::applyFilter('run', function($self, $params, $chain) {
	if (!Environment::is('production')) {
		return $chain->next($self, $params, $chain);
	}
	$key = md5(LITHIUM_APP_PATH) . '.core.libraries';

	if ($cache = Cache::read('default', $key)) {
		$cache = (array) $cache + Libraries::cache();
		Libraries::cache($cache);
	}
	$result = $chain->next($self, $params, $chain);

	if ($cache != Libraries::cache()) {
		Cache::write('default', $key, Libraries::cache(), '+1 day');
	}
	return $result;
});

?>