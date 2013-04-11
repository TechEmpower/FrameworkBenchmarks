<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2012, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\analysis;

use ReflectionClass;
use lithium\util\String;
use lithium\analysis\Inspector;

/**
 * The `Debugger` class provides basic facilities for generating and rendering meta-data about the
 * state of an application in its current context.
 */
class Debugger extends \lithium\core\StaticObject {

	/**
	 * Used for temporary closure caching.
	 *
	 * @see lithium\analysis\Debugger::_closureDef()
	 * @var array
	 */
	protected static $_closureCache = array();

	/**
	 * Outputs a stack trace based on the supplied options.
	 *
	 * @param array $options Format for outputting stack trace. Available options are:
	 *        - `'args'`: A boolean indicating if arguments should be included.
	 *        - `'depth'`: The maximum depth of the trace.
	 *        - `'format'`: Either `null`, `'points'` or `'array'`.
	 *        - `'includeScope'`: A boolean indicating if items within scope
	 *           should be included.
	 *        - `'scope'`: Scope for items to include.
	 *        - `'start'`: The depth to start with.
	 *        - `'trace'`: A trace to use instead of generating one.
	 * @return string Stack trace formatted according to `'format'` option.
	 */
	public static function trace(array $options = array()) {
		$defaults = array(
			'depth' => 999,
			'format' => null,
			'args' => false,
			'start' => 0,
			'scope' => array(),
			'trace' => array(),
			'includeScope' => true,
			'closures' => true
		);
		$options += $defaults;

		$backtrace = $options['trace'] ?: debug_backtrace();
		$scope = $options['scope'];
		$count = count($backtrace);
		$back = array();
		$traceDefault = array(
			'line' => '??', 'file' => '[internal]', 'class' => null, 'function' => '[main]'
		);

		for ($i = $options['start']; $i < $count && $i < $options['depth']; $i++) {
			$trace = array_merge(array('file' => '[internal]', 'line' => '??'), $backtrace[$i]);
			$function = '[main]';

			if (isset($backtrace[$i + 1])) {
				$next = $backtrace[$i + 1] + $traceDefault;
				$function = $next['function'];

				if (!empty($next['class'])) {
					$function = $next['class'] . '::' . $function . '(';
					if ($options['args'] && isset($next['args'])) {
						$args = array_map(array('static', 'export'), $next['args']);
						$function .= join(', ', $args);
					}
					$function .= ')';
				}
			}

			if ($options['closures'] && strpos($function, '{closure}') !== false) {
				$function = static::_closureDef($backtrace[$i], $function);
			}
			if (in_array($function, array('call_user_func_array', 'trigger_error'))) {
				continue;
			}
			$trace['functionRef'] = $function;

			if ($options['format'] === 'points' && $trace['file'] !== '[internal]') {
				$back[] = array('file' => $trace['file'], 'line' => $trace['line']);
			} elseif (is_string($options['format']) && $options['format'] != 'array') {
				$back[] = String::insert($options['format'], array_map(
					function($data) { return is_object($data) ? get_class($data) : $data; },
					$trace
				));
			} elseif (empty($options['format'])) {
				$back[] = $function . ' - ' . $trace['file'] . ', line ' . $trace['line'];
			} else {
				$back[] = $trace;
			}

			if (!empty($scope) && array_intersect_assoc($scope, $trace) == $scope) {
				if (!$options['includeScope']) {
					$back = array_slice($back, 0, count($back) - 1);
				}
				break;
			}
		}

		if ($options['format'] === 'array' || $options['format'] === 'points') {
			return $back;
		}
		return join("\n", $back);
	}

	/**
	 * Returns a parseable string representation of a variable.
	 *
	 * @param mixed $var The variable to export.
	 * @return string The exported contents.
	 */
	public static function export($var) {
		$export = var_export($var, true);

		if (is_array($var)) {
			$replace = array(" (", " )", "  ", " )", "=> \n\t");
			$with = array("(", ")", "\t", "\t)", "=> ");
			$export = str_replace($replace, $with, $export);
		}
		return $export;
	}

	/**
	 * Locates original location of closures.
	 *
	 * @param mixed $reference File or class name to inspect.
	 * @param integer $callLine Line number of class reference.
	 */
	protected static function _definition($reference, $callLine) {
		if (file_exists($reference)) {
			foreach (array_reverse(token_get_all(file_get_contents($reference))) as $token) {
				if (!is_array($token) || $token[2] > $callLine) {
					continue;
				}
				if ($token[0] === T_FUNCTION) {
					return $token[2];
				}
			}
			return;
		}
		list($class, $method) = explode('::', $reference);

		if (!class_exists($class)) {
			return;
		}

		$classRef = new ReflectionClass($class);
		$methodInfo = Inspector::info($reference);
		$methodDef = join("\n", Inspector::lines($classRef->getFileName(), range(
			$methodInfo['start'] + 1, $methodInfo['end'] - 1
		)));

		foreach (array_reverse(token_get_all("<?php {$methodDef} ?>")) as $token) {
			if (!is_array($token) || $token[2] > $callLine) {
				continue;
			}
			if ($token[0] === T_FUNCTION) {
				return $token[2] + $methodInfo['start'];
			}
		}
	}

	protected static function _closureDef($frame, $function) {
		$reference = '::';
		$frame += array('file' => '??', 'line' => '??');
		$cacheKey = "{$frame['file']}@{$frame['line']}";

		if (isset(static::$_closureCache[$cacheKey])) {
			return static::$_closureCache[$cacheKey];
		}

		if ($class = Inspector::classes(array('file' => $frame['file']))) {
			foreach (Inspector::methods(key($class), 'extents') as $method => $extents) {
				$line = $frame['line'];

				if (!($extents[0] <= $line && $line <= $extents[1])) {
					continue;
				}
				$class = key($class);
				$reference = "{$class}::{$method}";
				$function = "{$reference}()::{closure}";
				break;
			}
		} else {
			$reference = $frame['file'];
			$function = "{$reference}::{closure}";
		}
		$line = static::_definition($reference, $frame['line']) ?: '?';
		$function .= " @ {$line}";
		return static::$_closureCache[$cacheKey] = $function;
	}
}

?>