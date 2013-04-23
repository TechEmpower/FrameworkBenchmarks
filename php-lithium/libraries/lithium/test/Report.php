<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\test;

use lithium\core\Libraries;
use lithium\util\Inflector;
use lithium\core\ClassNotFoundException;

/**
 * This `Report` object aggregates tests in a group and allows you to run said tests to
 * obtain the results and stats (passes, fails, exceptions, skips) of the test run.
 *
 * While Lithium already comes with a text-based as well as web-based test interface, you
 * may use or extend the `Report` class to create your own test report functionality. In
 * addition, you can also create your own custom templates for displaying results in a different
 * format, such as json.
 *
 * Example usage, for built-in HTML format:
 *
 * {{{
 * $report = new Report(array(
 *     'title' => 'Test Report Title',
 *     'group' => new Group(array('data' => array('lithium\tests\cases\net\http\MediaTest'))),
 *     'format' => 'html'
 * ));
 *
 * $report->run();
 *
 * // Get the test stats:
 * $report->stats();
 *
 * // Get test results:
 * $report->results
 * }}}
 *
 * You may also choose to filter the results of the test runs to obtain additional information.
 * For example, say you wish to calculate the cyclomatic complexity of the classes you are testing:
 *
 * {{{
 * $report = new Report(array(
 *     'title' => 'Test Report Title',
 *     'group' => new Group(array('data' => array('lithium\tests\cases\net\http\MediaTest'))),
 *     'filters' => array('Complexity')
 * ));
 *
 * $report->run();
 *
 * // Get test results, including filter results:
 * $report->results
 * }}}
 *
 * @see lithium\test\Group
 * @see lithium\test\filter
 * @see lithium\test\templates
 */
class Report extends \lithium\core\Object {

	/**
	 * Contains an instance of `lithium\test\Group`, which contains all unit tests to be executed
	 * this test run.
	 *
	 * @see lithium\test\Group
	 * @var object
	 */
	public $group = null;

	/**
	 * Title of the group being run.
	 *
	 * @var string
	 */
	public $title;

	/**
	 * Group and filter results.
	 *
	 * @var array
	 */
	public $results = array('group' => array(), 'filters' => array());

	/**
	 * Start and end timers.
	 *
	 * @var array
	 */
	public $timer = array('start' => null, 'end' => null);

	/**
	 * An array key on fully-namespaced class names of the filter with options to be
	 * applied for the filter as the value
	 *
	 * @var array
	 */
	protected $_filters = array();

	/**
	 * Constructor.
	 *
	 * @param array $config Options array for the test run. Valid options are:
	 *        - `'group'`: The test group with items to be run.
	 *        - `'filters'`: An array of filters that the test output should be run through.
	 *        - `'format'`: The format of the template to use, defaults to `'txt'`.
	 *        - `'reporter'`: The reporter to use.
	 */
	public function __construct(array $config = array()) {
		$defaults = array(
			'title' => null,
			'group' => null,
			'filters' => array(),
			'format' => 'txt',
			'reporter' => null
		);
		parent::__construct($config + $defaults);
	}

	/**
	 * Initializer.
	 *
	 * @return void
	 */
	protected function _init() {
		$this->group = $this->_config['group'];
		$this->title = $this->_config['title'] ?: $this->_config['title'];
	}

	/**
	 * Runs tests.
	 *
	 * @return void
	 */
	public function run() {
		$tests = $this->group->tests();

		foreach ($this->filters() as $filter => $options) {
			$this->results['filters'][$filter] = array();
			$tests = $filter::apply($this, $tests, $options['apply']) ?: $tests;
		}
		$this->results['group'] = $tests->run(array(
			'reporter' => $this->_config['reporter']
		));

		foreach ($this->filters() as $filter => $options) {
			$this->results['filters'][$filter] = $filter::analyze($this, $options['analyze']);
		}
	}

	/**
	 * Collects Results from the test filters and aggregates them.
	 *
	 * @param string $class Classname of the filter for which to aggregate results.
	 * @param array $results Array of the filter results for
	 *              later analysis by the filter itself.
	 * @return void
	 */
	public function collect($class, $results) {
		$this->results['filters'][$class][] = $results;
	}

	/**
	 * Return statistics from the test runs.
	 *
	 * @return array
	 */
	public function stats() {
		$results = (array) $this->results['group'];
		$defaults = array(
			'asserts' => 0,
			'passes' => array(),
			'fails' => array(),
			'exceptions' => array(),
			'errors' => array(),
			'skips' => array()
		);
		$stats = array_reduce($results, function($stats, $result) use ($defaults) {
			$stats = (array) $stats + $defaults;
			$result = empty($result[0]) ? array($result) : $result;
			foreach ($result as $response) {
				if (empty($response['result'])) {
					continue;
				}
				$result = $response['result'];

				if (in_array($result, array('fail', 'exception'))) {
					$response = array_merge(
						array('class' => 'unknown', 'method' => 'unknown'), $response
					);
					$stats['errors'][] = $response;
				}
				unset($response['file'], $response['result']);

				if (in_array($result, array('pass', 'fail'))) {
					$stats['asserts']++;
				}
				if (in_array($result, array('pass', 'fail', 'exception', 'skip'))) {
					$stats[Inflector::pluralize($result)][] = $response;
				}
			}
			return $stats;
		});
		$stats = (array) $stats + $defaults;
		$count = array_map(
			function($value) { return is_array($value) ? count($value) : $value; }, $stats
		);
		$success = $count['passes'] === $count['asserts'] && $count['errors'] === 0;
		return compact('stats', 'count', 'success');
	}

	/**
	 * Renders the test output (e.g. layouts and filter templates).
	 *
	 * @param string $template name of the template (i.e. `'layout'`).
	 * @param string|array $data array from `_data()` method.
	 * @return string
	 * @filter
	 */
	public function render($template, $data = array()) {
		$config = $this->_config;

		if ($template === "stats" && !$data) {
			$data = $this->stats();
		}
		$template = Libraries::locate('test.templates', $template, array(
			'filter' => false, 'type' => 'file', 'suffix' => ".{$config['format']}.php"
		));
		$params = compact('template', 'data', 'config');

		return $this->_filter(__METHOD__, $params, function($self, $params, $chain) {
			extract($params['data']);
			ob_start();
			include $params['template'];
			return ob_get_clean();
		});
	}

	public function filters(array $filters = array()) {
		if ($this->_filters && !$filters) {
			return $this->_filters;
		}
		$filters += (array) $this->_config['filters'];
		$results = array();

		foreach ($filters as $filter => $options) {
			if (!$class = Libraries::locate('test.filter', $filter)) {
				throw new ClassNotFoundException("`{$class}` is not a valid test filter.");
			}
			$options['name'] = strtolower(join('', array_slice(explode("\\", $class), -1)));
			$results[$class] = $options + array('apply' => array(), 'analyze' => array());
		}
		return $this->_filters = $results;
	}
}

?>