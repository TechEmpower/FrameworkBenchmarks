<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\template;

use lithium\core\Libraries;
use lithium\template\TemplateException;

/**
 * As one of the three pillars of the Model-View-Controller design pattern, the `View` class
 * (along with other supporting classes) is responsible for taking the data passed from the
 * request and/or controller, inserting this into the requested template/layout, and then returning
 * the rendered content.
 *
 * The `View` class interacts with a variety of other classes in order to achieve maximum
 * flexibility and configurability at all points in the view rendering and presentation
 * process. The `Loader` class is tasked with locating and reading template files which are then
 * passed to the `Renderer` adapter subclass.
 *
 * In the default configuration, the `File` adapter acts as both renderer and loader, loading files
 * from paths defined in _process steps_ (described below) and rendering them as plain PHP files,
 * augmented with [special syntax](../template).
 *
 * The `View` class operates on _processes_, which define the steps to render a completed view. For
 * example, the default process, which renders a template wrapped in a layout, is comprised of two
 * _steps_: the first step renders the main template and captures it to the rendering context, where
 * it is embedded in the layout in the second step. See the `$_steps` and `$_processes` properties
 * for more information.
 *
 * Using steps and processes, you can create rendering scenarios to suit very complex needs.
 *
 * By default, the `View` class is called during the course of the framework's dispatch cycle by the
 * `Media` class. However, it is also possible to instantiate and call `View` directly, in cases
 * where you wish to bypass all other parts of the framework and simply return rendered content.
 *
 * A simple example, using the `Simple` renderer/loader for string templates:
 *
 * {{{
 * $view = new View(array('loader' => 'Simple', 'renderer' => 'Simple'));
 * echo $view->render('element', array('name' => "Robert"), array('element' => 'Hello, {:name}!'));
 *
 * // Output:
 * "Hello, Robert!";
 * }}}
 *
 *  _Note_: This is easily adapted for XML templating.
 *
 * Another example, this time of something that could be used in an application
 * error handler:
 *
 * {{{
 * $view = new View(array(
 *     'paths' => array(
 *         'template' => '{:library}/views/errors/{:template}.{:type}.php',
 *         'layout'   => '{:library}/views/layouts/{:layout}.{:type}.php',
 *     )
 * ));
 *
 * $page = $View->render('all', array('content' => $info), array(
 *     'template' => '404',
 *     'layout' => 'error'
 * ));
 * }}}
 *
 * To learn more about processes and process steps, see the `$_processes` and `$_steps` properties,
 * respectively.
 *
 * @see lithium\template\view\Renderer
 * @see lithium\template\view\adapter
 * @see lithium\net\http\Media
 */
class View extends \lithium\core\Object {

	/**
	 * Output filters for view rendering.
	 *
	 * @var array List of filters.
	 */
	public $outputFilters = array();

	/**
	 * Holds the details of the current request that originated the call to this view, if
	 * applicable.  May be empty if this does not apply.  For example, if the View class is
	 * created to render an email.
	 *
	 * @see lithium\action\Request
	 * @var object `Request` object instance.
	 */
	protected $_request = null;

	/**
	 * Holds a reference to the `Response` object that will be returned at the end of the current
	 * dispatch cycle. Allows headers and other response attributes to be assigned in the templating
	 * layer.
	 *
	 * @see lithium\action\Response
	 * @var object `Response` object instance.
	 */
	protected $_response = null;

	/**
	 * The object responsible for loading template files.
	 *
	 * @var object Loader object.
	 */
	protected $_loader = null;

	/**
	 * Object responsible for rendering output.
	 *
	 * @var objet Renderer object.
	 */
	protected $_renderer = null;

	/**
	 * Path used to look up view loading and rendering adapters.
	 *
	 * @var string
	 */
	protected $_adapters = 'adapter.template.view';

	/**
	 * View processes are aggregated lists of steps taken to to create a complete, rendered view.
	 * For example, the default process, `'all'`, renders a template, then renders a layout, using
	 * the rendered template content. A process can be defined using one or more steps defined in
	 * the `$_steps` property. Each process definition is a simple array of ordered values, where
	 * each value is a key in the `$_steps` array.
	 *
	 * @see lithium\template\View::$_steps
	 * @see lithium\template\View::render()
	 * @var array
	 */
	protected $_processes = array(
		'all' => array('template', 'layout'),
		'template' => array('template'),
		'element' => array('element')
	);

	/**
	 * The list of available rendering steps. Each step contains instructions for how to render one
	 * piece of a multi-step view rendering. The `View` class combines multiple steps into
	 * _processes_ to create the final output.
	 *
	 * Each step is named by its key in the `$_steps` array, and can have the following options:
	 *
	 * - `'path'` _string_: Indicates the set of paths to use when loading templates.
	 *
	 * - `'conditions'` _mixed_: Make the step dependent on a value being present, or on some other
	 *    arbitrary condition. If a `'conditions'` is a string, it indicates that a key with that
	 *    name must be present in the `$options` passed to `render()`, and must be set to a
	 *    non-empty value. If a closure, it will be executed with the rendering parameters, and must
	 *    return `true` or `false`. In either case, if the condition is satisfied, the step is
	 *    processed. Otherwise, it is skipped. See the `_conditions()` method for more information.
	 *
	 * - `'capture'` _array_: If specified, allows the results of this rendering step to be assigned
	 *   to a template variable used in subsequent steps, or to the templating context for use in
	 *   subsequent steps. If can be specified in the form of `array('context' => '<var-name>')` or
	 *   `array('data' => '<var-name>')`. If the `'context'` key is used, the results are captured
	 *   to the rendering context. Likewise with the `'data'` key, results are captured to a
	 *   template variable.
	 *
	 * - `'multi'` _boolean_: If set to `true`, the rendering parameter matching the name of this
	 *   step can be an array containing multiple values, in which case this step is executed
	 *   multiple times, once for each value of the array.
	 *
	 * @see lithium\template\View::$_processes
	 * @see lithium\template\View::render()
	 * @var array
	 */
	protected $_steps = array(
		'template' => array('path' => 'template', 'capture' => array('context' => 'content')),
		'layout' => array(
			'path' => 'layout', 'conditions' => 'layout', 'multi' => true, 'capture' => array(
				'context' => 'content'
			)
		),
		'element' => array('path' => 'element')
	);

	/**
	 * Auto-configuration parameters.
	 *
	 * @var array Objects to auto-configure.
	 */
	protected $_autoConfig = array(
		'request', 'response', 'processes' => 'merge', 'steps' => 'merge'
	);

	/**
	 * Constructor.
	 *
	 * @see lithium\template\View::$_steps
	 * @see lithium\template\View::$_processes
	 * @param array $config Class configuration parameters The available options are:
	 *         - `'loader'` _mixed_: Instance or name of the class used for locating and reading
	 *           template content. Defaults to `File`, which reads template content from PHP files.
	 *         - `'renderer'` _mixed_: Instance or name of the class that populates template
	 *           content with the data passed in to the view layer, typically from a controller.
	 *           Defaults to `'File'`, which executes templates as standard PHP files, using path
	 *           information returned from the `loader` class. Both `loader` and `renderer`
	 *           classes are looked up using the `'adapter.template.view'` path, which locates
	 *           classes in the `extensions\adapter\template\view` sub-namespace of an application
	 *           or plugin.
	 *         - `'request'`: The `Request` object to be made available in the templates.
	 *           Defaults to `null`.
	 *         - `'steps'` _array_: The array of step configurations to add to the built-in
	 *           configurations. Will be merged with the defaults, with any configurations passed
	 *           in overwriting built-in steps. See the `$_steps` property for more information.
	 *         - `'processes'` _array_: The array of process steps to add to the built-in
	 *           configurations. Will be merged with the defaults, with any configurations passed
	 *           in overwriting built-in processes. See the `$_processes` property for more
	 *           information.
	 *         - `'outputFilters'` _array_: An array of filters to be used when handling output. By
	 *           default, the class is initialized with one filter, `h`, which is used in automatic
	 *           output escaping.
	 */
	public function __construct(array $config = array()) {
		$defaults = array(
			'request' => null,
			'response' => null,
			'loader' => 'File',
			'renderer' => 'File',
			'steps' => array(),
			'processes' => array(),
			'outputFilters' => array()
		);
		parent::__construct($config + $defaults);
	}

	/**
	 * Perform initialization of the View.
	 *
	 * Looks up and initializes loader and renderer classes, and initializes the output escape
	 * handler, matching the encoding from the `Response` object.
	 *
	 * @return void
	 */
	protected function _init() {
		parent::_init();

		$encoding = 'UTF-8';

		if ($this->_response) {
			$encoding =& $this->_response->encoding;
		}
		$h = function($data) use (&$encoding) {
			return htmlspecialchars((string) $data, ENT_QUOTES, $encoding);
		};
		$this->outputFilters += compact('h') + $this->_config['outputFilters'];

		foreach (array('loader', 'renderer') as $key) {
			if (is_object($this->_config[$key])) {
				$this->{'_' . $key} = $this->_config[$key];
				continue;
			}
			$class = $this->_config[$key];
			$config = array('view' => $this) + $this->_config;
			$this->{'_' . $key} = Libraries::instance($this->_adapters, $class, $config);
		}
	}

	/**
	 * Executes a named rendering process by running each process step in sequence and aggregating
	 * the results. The `View` class comes with 3 built-in processes: `'all'`, `'template'`, and
	 * `'element'`. The `'all'` process is the default two-step rendered view, where a template is
	 * wrapped in a layout containing a header and footer.
	 *
	 * @see lithium\template\View::_conditions()
	 * @see lithium\template\View::$_processes
	 * @see lithium\template\View::$_steps
	 * @param string $process A named set of rendering steps defined in the `$_processes` array.
	 * @param array $data An associative array of data to be rendered in the set of templates.
	 * @param array $options Options used when rendering. Available keys are as follows:
	 *              - `'type'` _string_: The type of content to render. Defaults to `'html'`.
	 *              - `'layout'` _string_: The name of the layout to use in the default two-step
	 *                 rendering process. Defaults to `null`.
	 *              - `'template'` _string_: The name of the template to render. Defaults to `null`.
	 *              - `'context'` _array_: An associative array of information to inject into the
	 *                 rendering context.
	 *              - `'paths'` _array_: A nested array of paths to use for rendering steps. The
	 *                top-level keys should match the `'path'` key in a step configuration (i.e.:
	 *                `'template'`, `'layout'`, or `'element'`), and the second level is an array
	 *                of path template strings to search (can be a string if there's only one path).
	 *                These path strings generally take the following form:
	 *                `'{:library}/views/{:controller}/{:template}.{:type}.php'`. These template
	 *                strings are specific to the `File` loader, but can take any form useful to the
	 *                template loader being used.
	 * @return string Returns the result of the rendering process, typically by rendering a template
	 *         first, then rendering a layout (using the default configuration of the `'all'`
	 *         process).
	 */
	public function render($process, array $data = array(), array $options = array()) {
		$defaults = array(
			'type' => 'html',
			'layout' => null,
			'template' => null,
			'context' => array(),
			'paths' => array(),
			'data' => array()
		);
		$options += $defaults;

		$data += $options['data'];
		$paths = $options['paths'];
		unset($options['data'], $options['paths']);
		$params = array_filter($options, function($val) { return $val && is_string($val); });
		$result = null;

		foreach ($this->_process($process, $params) as $name => $step) {
			if (isset($paths[$name]) && $paths[$name] === false) {
				continue;
			}
			if (!$this->_conditions($step, $params, $data, $options)) {
				continue;
			}
			if ($step['multi'] && isset($options[$name])) {
				foreach ((array) $options[$name] as $value) {
					$params[$name] = $value;
					$result = $this->_step($step, $params, $data, $options);
				}
				continue;
			}
			$result = $this->_step((array) $step, $params, $data, $options);
		}
		return $result;
	}

	/**
	 * Evaluates a step condition to determine if the step should be executed.
	 *
	 * @see lithium\template\View::$_steps
	 * @param array $step The array of instructions that define a rendering step.
	 * @param array $params The parameters associated with this rendering operation, as passed to
	 *              `render()` (filtered from the `$options` parameter).
	 * @param array $data The associative array of template variables passed to `render()`.
	 * @param array $options The `$options` parameter, as passed to `render()`.
	 * @return boolean Returns `true` if the step should be executed, or `false` if the step should
	 *         be skipped. If the step array has a `'conditions'` key which is a string, it checks
	 *         to see if the rendering options (`$options`) contain a key of the same name, and if
	 *         that key evaluates to `true`. If `'conditions'` is a closure, that closure is
	 *         executed with the rendering parameters (`$params`, `$data`, and `$options`), and the
	 *         result is determined by the return value of the closure. If a step definition has no
	 *         `'conditions'` key, it is always executed.
	 */
	protected function _conditions(array $step, array $params, array $data, array $options) {
		if (!$conditions = $step['conditions']) {
			return true;
		}
		$isCallable = is_callable($conditions) && !is_string($conditions);
		if ($isCallable && !$conditions($params, $data, $options)) {
			return false;
		}
		if (is_string($conditions) && !(isset($options[$conditions]) && $options[$conditions])) {
			return false;
		}

		return true;
	}

	/**
	 * Performs a rendering step.
	 *
	 * @see lithium\template\view\adapter\File::template()
	 * @see lithium\template\view\Renderer::render()
	 * @see lithium\template\view\adapter\File::render()
	 * @param array $step The array defining the step configuration to render.
	 * @param array $params An associative array of string values used in the template lookup
	 *              process. See the `$params` argument of `File::template()`.
	 * @param array $data associative array for template data.
	 * @param array $options An associative array of options to pass to the renderer. See the
	 *              `$options` parameter of `Renderer::render()` or `File::render()`.
	 * @return string
	 * @filter
	 */
	protected function _step(array $step, array $params, array &$data, array &$options = array()) {
		$step += array('path' => null, 'capture' => null);
		$_renderer = $this->_renderer;
		$_loader = $this->_loader;
		$filters = $this->outputFilters;
		$params = compact('step', 'params', 'options') + array(
			'data' => $data + $filters,
			'loader' => $_loader,
			'renderer' => $_renderer
		);

		$filter = function($self, $params) {
			$template = $params['loader']->template($params['step']['path'], $params['params']);
			return $params['renderer']->render($template, $params['data'], $params['options']);
		};
		$result = $this->_filter(__METHOD__, $params, $filter);

		if (is_array($step['capture'])) {
			switch (key($step['capture'])) {
				case 'context':
					$options['context'][current($step['capture'])] = $result;
				break;
				case 'data':
					$data[current($step['capture'])] = $result;
				break;
			}
		}
		return $result;
	}

	/**
	 * Converts a process name to an array containing the rendering steps to be executed for each
	 * process.
	 *
	 * @param string $process A named set of rendering steps.
	 * @param array $params
	 * @return array A 2-dimensional array that defines the rendering process. The first dimension
	 *         is a numerically-indexed array containing each rendering step. The second dimension
	 *         represents the parameters for each step.
	 */
	protected function _process($process, &$params) {
		$defaults = array('conditions' => null, 'multi' => false);

		if (!is_array($process)) {
			if (!isset($this->_processes[$process])) {
				throw new TemplateException("Undefined rendering process '{$process}'.");
			}
			$process = $this->_processes[$process];
		}
		if (is_string(key($process))) {
			return $this->_convertSteps($process, $params, $defaults);
		}
		$result = array();

		foreach ($process as $step) {
			if (is_array($step)) {
				$result[] = $step + $defaults;
				continue;
			}
			if (!isset($this->_steps[$step])) {
				throw new TemplateException("Undefined rendering step '{$step}'.");
			}
			$result[$step] = $this->_steps[$step] + $defaults;
		}
		return $result;
	}

	/**
	 * Handles API backward compatibility by converting an array-based rendering instruction passed
	 * to `render()` as a process, to a set of rendering steps, rewriting any associated rendering
	 * parameters as necessary.
	 *
	 * @param array $command A deprecated rendering instruction, i.e.
	 *              `array('template' => '/path/to/template')`.
	 * @param array $params The array of associated rendering parameters, passed by reference.
	 * @param array $defaults Default step rendering options to be merged with the passed rendering
	 *              instruction information.
	 * @return array Returns a converted set of rendering steps, to be executed in `render()`.
	 */
	protected function _convertSteps(array $command, array &$params, $defaults) {
		if (count($command) === 1) {
			$params['template'] = current($command);
			return array(array('path' => key($command)) + $defaults);
		}
		return $command;
	}
}

?>