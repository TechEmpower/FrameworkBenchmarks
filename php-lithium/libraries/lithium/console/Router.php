<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\console;

/**
 * The `Router` class uses an instance of `lithium\console\Request`, which represents an incoming
 * command-line invocation, to parse the correct command, and sub-command(s) and parameters, which
 * are used by `lithium\console\Dispatcher` to load and execute the proper `Command` class.
 */
class Router extends \lithium\core\Object {

	/**
	 * Parse incoming request from console. Short and long (GNU-style) options
	 * in the form of `-f`, `--foo`, `--foo-bar` and `--foo=bar` are parsed.
	 * XF68-style long options (i.e. `-foo`) are not supported but support
	 * can be added by extending this class.
	 *
	 * @param object $request lithium\console\Request
	 * @return array $params
	 */
	public static function parse($request = null) {
		$defaults = array('command' => null, 'action' => 'run', 'args' => array());
		$params = $request ? (array) $request->params + $defaults : $defaults;

		if (!empty($request->argv)) {
			$args = $request->argv;

			while ($arg = array_shift($args)) {
				if (preg_match('/^-(?P<key>[a-zA-Z0-9])$/i', $arg, $match)) {
					$params[$match['key']] = true;
					continue;
				}
				if (preg_match('/^--(?P<key>[a-z0-9-]+)(?:=(?P<val>.+))?$/i', $arg, $match)) {
					$params[$match['key']] = !isset($match['val']) ? true : $match['val'];
					continue;
				}
				$params['args'][] = $arg;
			}
		}
		foreach (array('command', 'action') as $param) {
			if (!empty($params['args'])) {
				$params[$param] = array_shift($params['args']);
			}
		}
		return $params;
	}
}

?>