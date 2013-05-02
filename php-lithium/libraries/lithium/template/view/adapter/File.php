<?php
/**
 * Lithium: the most rad php framework
 *
 * @copyright     Copyright 2013, Union of RAD (http://union-of-rad.org)
 * @license       http://opensource.org/licenses/bsd-license.php The BSD License
 */

namespace lithium\template\view\adapter;

use lithium\util\String;
use lithium\core\Libraries;
use lithium\template\TemplateException;

/**
 * The File adapter implements both template loading and rendering, and uses the
 * `lithium\template\view\Stream` class or `lithium\template\view\Compiler` class to auto-escape
 * template output with short tags (i.e. `<?=`).
 *
 * For more information about implementing your own template loaders or renderers, see the
 * `lithium\template\View` class.
 *
 * @see lithium\template\View
 * @see lithium\template\view\Compiler
 */
class File extends \lithium\template\view\Renderer implements \ArrayAccess {

	/**
	 * These configuration variables will automatically be assigned to their corresponding protected
	 * properties when the object is initialized.
	 *
	 * @var array
	 */
	protected $_autoConfig = array(
		'classes' => 'merge', 'request', 'response', 'context',
		'strings', 'handlers', 'view', 'compile', 'paths'
	);

	/**
	 * Boolean flag indicating whether templates should be pre-compiled before inclusion. For more
	 * information on template compilation, see `view\Compiler`.
	 *
	 * @see lithium\template\view\Compiler
	 * @var boolean
	 */
	protected $_compile = true;

	/**
	 * An array containing the variables currently in the scope of the template. These values are
	 * manipulable using array syntax against the template object, i.e. `$this['foo'] = 'bar'`
	 * inside your template files.
	 *
	 * @var array
	 */
	protected $_data = array();

	/**
	 * Variables that have been set from a view/element/layout/etc. that should be available to the
	 * same rendering context.
	 *
	 * @var array Key/value pairs of variables
	 */
	protected $_vars = array();

	protected $_paths = array();

	/**
	 * `File`'s dependencies. These classes are used by the output handlers to generate URLs
	 * for dynamic resources and static assets, as well as compiling the templates.
	 *
	 * @see Renderer::$_handlers
	 * @var array
	 */
	protected $_classes = array(
		'compiler' => 'lithium\template\view\Compiler',
		'router' => 'lithium\net\http\Router',
		'media'  => 'lithium\net\http\Media'
	);

	public function __construct(array $config = array()) {
		$defaults = array(
			'classes' => array(),
			'compile' => true,
			'compiler' => array(),
			'extract' => true,
			'paths' => array()
		);
		parent::__construct($config + $defaults);
	}

	/**
	 * Renders content from a template file provided by `template()`.
	 *
	 * @param string $template
	 * @param array|string $data
	 * @param array $options
	 * @return string
	 */
	public function render($template, $data = array(), array $options = array()) {
		$defaults = array('context' => array());
		$options += $defaults;

		$this->_context = $options['context'] + $this->_context;
		$this->_data = (array) $data + $this->_vars;
		$this->_options = $options;
		$template__ = $template;
		unset($options, $template, $defaults, $data);

		if ($this->_config['extract']) {
			extract($this->_data, EXTR_OVERWRITE);
		} elseif ($this->_view) {
			extract((array) $this->_view->outputFilters, EXTR_OVERWRITE);
		}

		ob_start();
		include $template__;
		return ob_get_clean();
	}

	/**
	 * Returns a template file name
	 *
	 * @param string $type
	 * @param array $params
	 * @return string
	 */
	public function template($type, array $params) {
		$library = Libraries::get(isset($params['library']) ? $params['library'] : true);
		$params['library'] = $library['path'];
		$path = $this->_paths($type, $params);

		if ($this->_compile) {
			$compiler = $this->_classes['compiler'];
			$path = $compiler::template($path, $this->_config['compiler']);
		}
		return $path;
	}

	/**
	 * Allows checking to see if a value is set in template data, i.e. `$this['foo']` in templates.
	 *
	 * @param string $offset The key / variable name to check.
	 * @return boolean Returns `true` if the value is set, otherwise `false`.
	 */
	public function offsetExists($offset) {
		return array_key_exists($offset, $this->_data);
	}

	public function offsetGet($offset) {
		return isset($this->_data[$offset]) ? $this->_data[$offset] : null;
	}

	public function offsetSet($offset, $value) {
		$this->_data[$offset] = $value;
	}

	public function offsetUnset($offset) {
		unset($this->_data[$offset]);
	}

	/**
	 * Searches one or more path templates for a matching template file, and returns the file name.
	 *
	 * @param string $type
	 * @param array $params The set of options keys to be interpolated into the path templates
	 *              when searching for the correct file to load.
	 * @return string Returns the first template file found. Throws an exception if no templates
	 *         are available.
	 */
	protected function _paths($type, array $params) {
		if (!isset($this->_paths[$type])) {
			throw new TemplateException("Invalid template type '{$type}'.");
		}

		foreach ((array) $this->_paths[$type] as $path) {
			if (!file_exists($path = String::insert($path, $params))) {
				continue;
			}
			return $path;
		}
		throw new TemplateException("Template not found at path `{$path}`.");
	}
}

?>