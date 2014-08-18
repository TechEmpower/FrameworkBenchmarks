<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\test\filter;

/**
 * The `Profiler` filter tracks timing and memory usage information for each test method, and
 * presents aggregate reports across single test runs. Used for performance-tuning classes and
 * methods.
 */
class Profiler extends \lithium\test\Filter {

	/**
	 * Contains the list of profiler checks to run against each test.  Values can be string
	 * function names, arrays containing function names as the first key and function parameters
	 * as subsequent keys, or closures.
	 *
	 * @var array
	 * @see lithium\test\Profiler::check()
	 */
	protected static $_metrics = array(
		'Time' => array(
			'function' => array('microtime', true),
			'format' => 'seconds'
		),
		'Current Memory' => array(
			'function' => 'memory_get_usage',
			'format' => 'bytes'
		),
		'Peak Memory' => array(
			'function' => 'memory_get_peak_usage',
			'format' => 'bytes'
		),
		'Current Memory (Xdebug)' => array(
			'function' => 'xdebug_memory_usage',
			'format' => 'bytes'
		),
		'Peak Memory (Xdebug)' => array(
			'function' => 'xdebug_peak_memory_usage',
			'format' => 'bytes'
		)
	);

	protected static $_formatters = array();

	/**
	 * Verifies that the corresponding function exists for each built-in profiler check.
	 * Initializes display formatters.
	 *
	 * @return void
	 */
	public static function __init() {
		foreach (static::$_metrics as $name => $check) {
			$function = current((array) $check['function']);

			if (is_string($check['function']) && !function_exists($check['function'])) {
				unset(static::$_metrics[$name]);
			}
		}

		static::$_formatters = array(
			'seconds' => function($value) { return number_format($value, 4) . 's'; },
			'bytes' => function($value) { return number_format($value / 1024, 3) . 'k'; }
		);
	}

	/**
	 * Takes an instance of an object (usually a Collection object) containing test
	 * instances. Allows for preparing tests before they are run.
	 *
	 * @param object $report Instance of Report which is calling apply.
	 * @param array $tests The test to apply this filter on
	 * @param array $options Options for how this filter should be applied. Available options are:
	 *              - `'method'`
	 *              - `'run'`
	 *              - `'checks'`
	 * @return object Returns the instance of `$tests`.
	 */
	public static function apply($report, $tests, array $options = array()) {
		$defaults = array('method' => 'run', 'checks' => static::$_metrics);
		$options += $defaults;
		$m = $options['method'];
		$filter = function($self, $params, $chain) use ($report, $options) {
			$start = $results = array();

			$runCheck = function($check) {
				switch (true) {
					case (is_object($check) || is_string($check)):
						return $check();
					break;
					case (is_array($check)):
						$function = array_shift($check);
						$result = !$check ? $check() : call_user_func_array($function, $check);
					break;
				}
				return $result;
			};

			foreach ($options['checks'] as $name => $check) {
				$start[$name] = $runCheck($check['function']);
			}
			$methodResult = $chain->next($self, $params, $chain);

			foreach ($options['checks'] as $name => $check) {
				$results[$name] = $runCheck($check['function']) - $start[$name];
			}
			$report->collect(
				__CLASS__,
				array(
					$self->subject() => $results,
					'options' => $options + array('test' => get_class($self)),
					'method' => $params['method']
				)
			);
			return $methodResult;
		};
		$tests->invoke('applyFilter', array($m, $filter));
		return $tests;
	}

	/**
	 * Analyzes the results of a test run and returns the result of the analysis.
	 *
	 * @param object $report The report instance running this filter and aggregating results
	 * @param array $options Not used.
	 * @return array The results of the analysis.
	 */
	public static function analyze($report, array $options = array()) {
		$results = $report->results['group'];
		$collectedResults = static::collect($report->results['filters'][__CLASS__]);
		extract($collectedResults, EXTR_OVERWRITE);
		$metrics = array();

		foreach ($results as $testCase) {
			foreach ((array) $testCase as $assertion) {
				if ($assertion['result'] !== 'pass' && $assertion['result'] !== 'fail') {
					continue;
				}
				$class = $classMap[$assertion['class']];

				if (!isset($metrics[$class])) {
					$metrics[$class] = array('assertions' => 0);
				}
				$metrics[$class]['assertions']++;
			}
		}

		foreach ($filterResults as $class => $methods) {
			foreach ($methods as $methodName => $timers) {
				foreach ($timers as $title => $value) {
					if (!isset($metrics[$class][$title])) {
						$metrics[$class][$title] = 0;
					}
					$metrics[$class][$title] += $value;
				}
			}
		}

		$totals = array();
		foreach ($metrics as $class => $data) {
			foreach ($data as $title => $value) {
				if (isset(static::$_metrics[$title])) {
					if (isset($totals[$title]['value'])) {
						$totals[$title]['value'] += $value;
					} else {
						$totals[$title]['value'] = $value;
					}

					if (!isset($totals[$title]['format'])) {
						$format = static::$_metrics[$title]['format'];
						$totals[$title]['formatter'] = static::$_formatters[$format];
					}
				}
			}
		}

		$metrics['totals'] = $totals;
		return $metrics;
	}

	/**
	 * Add, remove, or modify a profiler check.
	 *
	 * @see lithium\test\Profiler::$_metrics
	 * @param mixed $name
	 * @param string $value
	 * @return mixed
	 */
	public function check($name, $value = null) {
		if (is_null($value) && !is_array($name)) {
			return isset(static::$_metrics[$name]) ? static::$_metrics[$name] : null;
		}

		if ($value === false) {
			unset(static::$_metrics[$name]);
			return;
		}

		if (!empty($value)) {
			static::$_metrics[$name] = $value;
		}

		if (is_array($name)) {
			static::$_metrics = $name + static::$_metrics;
		}
	}

	/**
	 * Collects the raw filter results and packages them for analysis.
	 *
	 * @param array $filterResults The results of the filter on the test run.
	 * @return array The packaged filter results prepared for analysis.
	 */
	public static function collect($filterResults) {
		$defaults = array('test' => null);
		$classMap = array();
		$packagedResults = array();

		foreach ($filterResults as $results) {
			$class = key($results);
			$options = $results['options'];
			$options += $defaults;
			$method = $results['method'];

			$classMap[$options['test']] = $class;
			if (!isset($packagedResults[$class])) {
				$packagedResults[$class] = array();
			}
			$packagedResults[$class][$method] = $results[$class];
		}

		$filterResults = $packagedResults;

		return array(
			'filterResults' => $filterResults,
			'classMap' => $classMap
		);
	}
}

?>