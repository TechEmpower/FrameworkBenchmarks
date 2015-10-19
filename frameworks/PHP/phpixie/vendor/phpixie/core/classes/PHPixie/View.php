<?php

namespace PHPixie;

/**
 * Manages passing variables to templates and rendering them
 * @package Core
 */
class View
{
	/**
	 * Pixie Dependancy Container
	 * @var \PHPixie\Pixie
	 */
	protected $pixie;
	
	/**
	 * Full path to template file
	 * @var string
	 */
	protected $path;

	/**
	 * The name of the view.
	 * @var string
	 */
	public $name;

	/**
	 * Stores all the variables passed to the view
	 * @var array
	 */
	protected $_data = array();

	/**
	 * File extension of the templates
	 * @var string
	 */
	protected $_extension = 'php';

	/**
	 * Constructs the view
	 *
	 * @param \PHPixie\Pixie $pixie Pixie dependency container
	 * @param string   $name The name of the template to use
	 * @throws \Exception If specified template is not found
	 */
	public function __construct($pixie, $name)
	{
		$this->pixie = $pixie;
		$this->name = $name;
		$file = $this->pixie->find_file('views', $name, $this->_extension);
			
		if ($file == false)
			throw new \Exception("View {$name} not found.");

		$this->path = $file;
	}

	/**
	 * Manages storing the data passed to the view as properties
	 *
	 * @param string $key Property name
	 * @param string $val Property value
	 * @return void
	 */
	public function __set($key, $val)
	{
		$this->_data[$key] = $val;
	}

	/**
	 * Manages accessing passed data as properties
	 *
	 * @param string   $key Property name
	 * @return mixed	Property value
	 * @throws \Exception If the property is not found
	 */
	public function __get($key)
	{
		if (isset($this->_data[$key]))
			return $this->_data[$key];
		throw new \Exception("Value {$key} not set for view {$this->name}");
	}

	/**
	 * Renders the template, all dynamically set properties
	 * will be available inside the view file as variables.
	 * Example:
	 * <code>
	 * $view = $this->pixie->view('frontpage');
	 * $view->title = "Page title";
	 * echo $view->render();
	 * </code>
	 *
	 * @return string Rendered template
	 */
	public function render()
	{
		extract($this->_data);
		ob_start();
		include($this->path);
		return ob_get_clean();
	}
	
}
