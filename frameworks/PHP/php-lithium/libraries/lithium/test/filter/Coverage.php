<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\test\filter;

use RuntimeException;
use lithium\core\Libraries;
use lithium\analysis\Inspector;

/**
 * Runs code coverage analysis for the executed tests.
 */
class Coverage extends \lithium\test\Filter {

	/**
	 * Takes an instance of an object (usually a Collection object) containing test
	 * instances. Attaches code coverage filtering to test cases.
	 *
	 * @see lithium\test\filter\Coverage::collect()
	 * @param object $report Instance of Report which is calling apply.
	 * @param array $tests The test to apply this filter on
	 * @param array $options Options for how code coverage should be applied. These options are
	 *              also passed to `Coverage::collect()` to determine how to aggregate results. See
	 *              the documentation for `collect()` for further options.  Options affecting this
	 *              method are:
	 *              -'method': The name of method to attach to, defaults to 'run'.
	 * @return object Returns the instance of `$tests` with code coverage analysis
	 *                     triggers applied.
	 */
	public static function apply($report, $tests, array $options = array()) {
		$defaults = array('method' => 'run');
		$options += $defaults;

		if (!function_exists('xdebug_start_code_coverage')) {
			$msg = "Xdebug not installed. Please install Xdebug before running code coverage.";
			throw new RuntimeException($msg);
		}

		$filter = function($self, $params, $chain) use ($report, $options) {
			xdebug_start_code_coverage(XDEBUG_CC_UNUSED | XDEBUG_CC_DEAD_CODE);
			$chain->next($self, $params, $chain);
			$results = xdebug_get_code_coverage();
			xdebug_stop_code_coverage();
			$report->collect(__CLASS__, array($self->subject() => $results));
		};
		$tests->invoke('applyFilter', array($options['method'], $filter));
		return $tests;
	}

	/**
	 * Analyzes code coverage results collected from XDebug, and performs coverage density analysis.
	 *
	 * @param object $report The report instance running this filter and aggregating results
	 * @param array $classes A list of classes to analyze coverage on. By default, gets all
	 *              defined subclasses of lithium\test\Unit which are currently in memory.
	 * @return array Returns an array indexed by file and line, showing the number of
	 *                    instances each line was called.
	 */
	public static function analyze($report, array $classes = array()) {
		$data = static::collect($report->results['filters'][__CLASS__]);
		$classes = $classes ?: array_filter(get_declared_classes(), function($class) use ($data) {
			$unit = 'lithium\test\Unit';
			return (!(is_subclass_of($class, $unit)) || array_key_exists($class, $data));
		});
		$classes = array_values(array_intersect((array) $classes, array_keys($data)));
		$densities = $result = array();

		foreach ($classes as $class) {
			$classMap = array($class => Libraries::path($class));
			$densities += static::_density($data[$class], $classMap);
		}
		$executableLines = array();

		if ($classes) {
			$executableLines = array_combine($classes, array_map(
				function($cls) { return Inspector::executable($cls, array('public' => false)); },
				$classes
			));
		}

		foreach ($densities as $class => $density) {
			$executable = $executableLines[$class];
			$covered = array_intersect(array_keys($density), $executable);
			$uncovered = array_diff($executable, $covered);
			if (count($executable)) {
				$percentage = round(count($covered) / (count($executable) ?: 1), 4) * 100;
			} else {
				$percentage = 100;
			}
			$result[$class] = compact('class', 'executable', 'covered', 'uncovered', 'percentage');
		}

		$result = static::collectLines($result);
		return $result;
	}

	/**
	 * Takes the raw line numbers and returns results with the code from
	 * uncovered lines included.
	 *
	 * @param array $result The raw line number results
	 * @return array
	 */
	public static function collectLines($result) {
		$output = null;
		$aggregate = array('covered' => 0, 'executable' => 0);

		foreach ($result as $class => $coverage) {
			$out = array();
			$file = Libraries::path($class);

			$aggregate['covered'] += count($coverage['covered']);
			$aggregate['executable'] += count($coverage['executable']);

			$uncovered = array_flip($coverage['uncovered']);
			$contents = explode("\n", file_get_contents($file));
			array_unshift($contents, ' ');
			$count = count($contents);

			for ($i = 1; $i <= $count; $i++) {
				if (isset($uncovered[$i])) {
					if (!isset($out[$i - 2])) {
						$out[$i - 2] = array(
							'class' => 'ignored',
							'data' => '...'
						);
					}
					if (!isset($out[$i - 1])) {
						$out[$i - 1] = array(
							'class' => 'covered',
							'data' => $contents[$i - 1]
						);
					}
					$out[$i] = array(
						'class' => 'uncovered',
						'data' => $contents[$i]
					);

					if (!isset($uncovered[$i + 1])) {
						$out[$i + 1] = array(
							'class' => 'covered',
							'data' => $contents[$i + 1]
						);
					}
				} elseif (
					isset($out[$i - 1]) && $out[$i - 1]['data'] !== '...' &&
					!isset($out[$i]) && !isset($out[$i + 1])
				) {
					$out[$i] = array(
						'class' => 'ignored',
						'data' => '...'
					);
				}
			}
			$result[$class]['output'][$file] = $out;
		}
		return $result;
	}

	/**
	 * Collects code coverage analysis results from `xdebug_get_code_coverage()`.
	 *
	 * @see lithium\test\Coverage::analyze()
	 * @param array $filterResults An array of results arrays from `xdebug_get_code_coverage()`.
	 * @param array $options Set of options defining how results should be collected.
	 * @return array The packaged filter results.
	 * @todo Implement $options['merging']
	 */
	public static function collect($filterResults, array $options = array()) {
		$defaults = array('merging' => 'class');
		$options += $defaults;
		$packagedResults = array();

		foreach ($filterResults as $results) {
			$class = key($results);
			$results = $results[$class];
			foreach ($results as $file => $lines) {
				unset($results[$file][0]);
			}

			switch ($options['merging']) {
				case 'class':
				default:
					if (!isset($packagedResults[$class])) {
						$packagedResults[$class] = array();
					}
					$packagedResults[$class][] = $results;
				break;
			}
		}

		return $packagedResults;
	}

	/**
	 * Reduces the results of multiple XDebug code coverage runs into a single 2D array of the
	 * aggregate line coverage density per file.
	 *
	 * @param array $runs An array containing multiple runs of raw XDebug coverage data, where
	 *              each array key is a file name, and it's value is XDebug's coverage
	 *              data for that file.
	 * @param array $classMap An optional map with class names as array keys and corresponding file
	 *              names as values. Used to filter the returned results, and will cause the array
	 *              keys of the results to be class names instead of file names.
	 * @return array
	 */
	protected static function _density($runs, $classMap = array()) {
		$results = array();

		foreach ($runs as $run) {
			foreach ($run as $file => $coverage) {
				if ($classMap) {
					if (!$class = array_search($file, $classMap)) {
						continue;
					}
					$file = $class;
				}
				if (!isset($results[$file])) {
					$results[$file] = array();
				}
				$coverage = array_filter($coverage, function($line) { return ($line === 1); });

				foreach ($coverage as $line => $isCovered) {
					if (!isset($results[$file][$line])) {
						$results[$file][$line] = 0;
					}
					$results[$file][$line]++;
				}
			}
		}
		return $results;
	}
}

?>